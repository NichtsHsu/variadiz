use std::collections::HashMap;

use nanoid::nanoid;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2, TokenTree as TokenTree2};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, parse_quote, punctuated::Pair, spanned::Spanned, visit_mut::VisitMut,
    Attribute, Expr, ExprBlock, FnArg, GenericParam, Generics, Ident, ImplItem, ImplItemFn, Item,
    ItemFn, ItemImpl, ItemStruct, Meta, Pat, PatType, Signature, Stmt, Token, Type, TypeParam,
    WhereClause, WherePredicate,
};

const ALPHABETS: [char; 62] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4',
    '5', '6', '7', '8', '9',
];

struct VariadicParameter {
    generic_parameter: TypeParam,
    generic_where_clause: Option<WhereClause>,
    parameter_ident: Ident,
    parameter_pat_type: PatType,
    parameter_ref_pat_type: PatType,
    parameter_mut_pat_type: PatType,
    not_variadic_generics: Generics,
    all_generics: Generics,
    fn_bounds_not_variadic: Option<WhereClause>,
}

struct LocalTraits {
    mapper: Ident,
    mapper_ref: Ident,
    mapper_mut: Ident,
    foreach: Ident,
    impls: TokenStream2,
}

struct CaptureVariableBind {
    variable: Ident,
    value: Expr,
}

struct CaptureVariableBindList(Vec<CaptureVariableBind>);

struct VariadicExpander<'a> {
    err: Option<syn::Error>,
    parameter: &'a VariadicParameter,
    local_traits: &'a LocalTraits,
}

impl VariadicParameter {
    fn is_variadic_related(input: TokenStream2, searched: &Ident) -> bool {
        input.into_iter().any(|tt| match tt {
            TokenTree2::Ident(i) => &i == searched,
            TokenTree2::Group(g) => Self::is_variadic_related(g.stream(), searched),
            _ => false,
        })
    }

    fn with_fn_signature(sig: &Signature) -> syn::Result<Self> {
        if sig.inputs.is_empty() {
            return Err(syn::Error::new(
                sig.inputs.span(),
                "requires at least one parameter that is not a receiver",
            ));
        }
        let last_parameter = sig.inputs.last().cloned().unwrap();
        let FnArg::Typed(parameter_pat_type) = last_parameter else {
            return Err(syn::Error::new(
                last_parameter.span(),
                "requires at least one parameter that is not a receiver",
            ));
        };
        let Pat::Ident(ident) = &*parameter_pat_type.pat else {
            return Err(syn::Error::new(
                parameter_pat_type.span(),
                "does not support pattern on variadic parameter",
            ));
        };
        let parameter_ident = ident.ident.clone();
        let parameter_type = (*parameter_pat_type.ty).clone();
        let parameter_ref_type = parse_quote!(&#parameter_type);
        let parameter_mut_type = parse_quote!(&mut #parameter_type);
        let mut parameter_ref_pat_type = parameter_pat_type.clone();
        parameter_ref_pat_type.ty = parameter_ref_type;
        let mut parameter_mut_pat_type = parameter_pat_type.clone();
        parameter_mut_pat_type.ty = parameter_mut_type;

        let mut not_variadic_generics = sig.generics.clone();
        let Some(GenericParam::Type(generic_parameter)) =
            not_variadic_generics.params.pop().map(Pair::into_value)
        else {
            return Err(syn::Error::new(
                sig.generics.params.span(),
                "requires generic type parameter for variadic",
            ));
        };
        let mut generic_where_clause = None;
        if let Some(mut where_clause) = not_variadic_generics.where_clause {
            let (variadic_generics_predicates, not_variadic_generics_predicates) =
                where_clause.predicates.into_iter().partition(|pred| {
                    if let WherePredicate::Type(type_pred) = pred {
                        Self::is_variadic_related(
                            type_pred.to_token_stream(),
                            &generic_parameter.ident,
                        )
                    } else {
                        false
                    }
                });
            where_clause.predicates = not_variadic_generics_predicates;
            not_variadic_generics.where_clause = Some(where_clause);
            if !variadic_generics_predicates.is_empty() {
                generic_where_clause = Some(WhereClause {
                    where_token: <Token![where]>::default(),
                    predicates: variadic_generics_predicates,
                });
            }
        }
        let fn_bounds_not_variadic = not_variadic_generics.where_clause.clone();

        Ok(Self {
            generic_parameter,
            generic_where_clause,
            parameter_ident,
            parameter_pat_type,
            parameter_ref_pat_type,
            parameter_mut_pat_type,
            not_variadic_generics,
            all_generics: sig.generics.clone(),
            fn_bounds_not_variadic,
        })
    }

    fn with_item_fn(item_fn: &ItemFn) -> syn::Result<Self> {
        Self::with_fn_signature(&item_fn.sig)
    }

    fn with_impl_item_fn(
        impl_item_fn: &ImplItemFn,
        common_generics: &Generics,
    ) -> syn::Result<Self> {
        let mut this = Self::with_fn_signature(&impl_item_fn.sig)?;
        this.not_variadic_generics
            .params
            .extend(common_generics.params.clone());
        if let Some(where_clause) = common_generics.where_clause.clone() {
            this.not_variadic_generics
                .make_where_clause()
                .predicates
                .extend(where_clause.predicates)
        }
        this.all_generics
            .params
            .extend(common_generics.params.clone());
        if let Some(where_clause) = common_generics.where_clause.clone() {
            this.all_generics
                .make_where_clause()
                .predicates
                .extend(where_clause.predicates)
        }

        Ok(this)
    }
}

impl LocalTraits {
    fn new(parameter: &VariadicParameter) -> (Self, WherePredicate) {
        let (not_variadic_impl_generics, not_variadic_type_generics, not_variadic_where_clause) =
            parameter.not_variadic_generics.split_for_impl();
        let variadic_generic_param = &parameter.generic_parameter;
        let variadic_generic_type = &variadic_generic_param.ident;
        let variadic_generic_param: Generics = parse_quote!(<#variadic_generic_param>);
        let variadic_parameter_type = &parameter.parameter_pat_type.ty;
        let variadic_parameter = &parameter.parameter_pat_type;
        let variadic_parameter_ref = &parameter.parameter_ref_pat_type;
        let variadic_parameter_mut = &parameter.parameter_mut_pat_type;
        let variadic_where_clause = &parameter.generic_where_clause;

        let mapper = Ident::new(
            &format!("__Mapper{}", nanoid!(16, &ALPHABETS)),
            Span::mixed_site(),
        );
        let mapper_fully_qualified = if parameter.not_variadic_generics.params.is_empty() {
            quote!(#mapper)
        } else {
            quote!(#mapper:: #not_variadic_type_generics)
        };
        let mapper_ref = Ident::new(
            &format!("__MapperRef{}", nanoid!(16, &ALPHABETS)),
            Span::mixed_site(),
        );
        let mapper_ref_fully_qualified = if parameter.not_variadic_generics.params.is_empty() {
            quote!(#mapper_ref)
        } else {
            quote!(#mapper_ref:: #not_variadic_type_generics)
        };
        let mapper_mut = Ident::new(
            &format!("__MapperMut{}", nanoid!(16, &ALPHABETS)),
            Span::mixed_site(),
        );
        let mapper_mut_fully_qualified = if parameter.not_variadic_generics.params.is_empty() {
            quote!(#mapper_mut)
        } else {
            quote!(#mapper_mut:: #not_variadic_type_generics)
        };

        let foreach = Ident::new(
            &format!("__Foreach{}", nanoid!(16, &ALPHABETS)),
            Span::mixed_site(),
        );
        let foreach_fully_qualified = if parameter.not_variadic_generics.params.is_empty() {
            quote!(#foreach)
        } else {
            quote!(#foreach::#not_variadic_type_generics)
        };
        let generic_f = Ident::new(
            &format!("__F{}", nanoid!(16, &ALPHABETS)),
            Span::mixed_site(),
        );

        let mut tuple_generics = parameter.all_generics.clone();
        let generic_others = Ident::new(
            &format!("__Others{}", nanoid!(16, &ALPHABETS)),
            Span::mixed_site(),
        );
        tuple_generics.params.push(parse_quote!(#generic_others));
        tuple_generics
            .make_where_clause()
            .predicates
            .push(parse_quote! {
                #generic_others: #foreach #not_variadic_type_generics
            });
        let (tuple_impl_generics, _, tuple_where_clause) = tuple_generics.split_for_impl();

        let impls = quote! {
            #[doc(hidden)]
            trait #mapper #not_variadic_impl_generics
            #not_variadic_where_clause
            {
                #[doc(hidden)]
                #[allow(unused)]
                fn map #variadic_generic_param (
                    &mut self,
                    #variadic_parameter
                ) #variadic_where_clause
                {
                }
            }

            #[doc(hidden)]
            trait #mapper_ref #not_variadic_impl_generics
            #not_variadic_where_clause
            {
                #[doc(hidden)]
                #[allow(unused)]
                fn map_ref #variadic_generic_param (
                    &mut self,
                    #variadic_parameter_ref
                ) #variadic_where_clause
                {
                }
            }

            #[doc(hidden)]
            trait #mapper_mut #not_variadic_impl_generics
            #not_variadic_where_clause
            {
                #[doc(hidden)]
                #[allow(unused)]
                fn map_mut #variadic_generic_param (
                    &mut self,
                    #variadic_parameter_mut
                ) #variadic_where_clause
                {
                }
            }

            #[doc(hidden)]
            trait #foreach #not_variadic_impl_generics: ::tuplez::TupleLike
            #not_variadic_where_clause
            {
                #[doc(hidden)]
                fn foreach<#generic_f>(
                    self,
                    f: &mut #generic_f
                ) where
                    #generic_f: #mapper #not_variadic_type_generics;

                #[doc(hidden)]
                fn foreach_ref<#generic_f>(
                    &self,
                    f: &mut #generic_f
                ) where
                    #generic_f: #mapper_ref #not_variadic_type_generics;

                #[doc(hidden)]
                fn foreach_mut<#generic_f>(
                    &mut self,
                    f: &mut #generic_f
                ) where
                    #generic_f: #mapper_mut #not_variadic_type_generics;
            }

            impl #not_variadic_impl_generics
            #foreach #not_variadic_type_generics
            for ::tuplez::Unit
            #not_variadic_where_clause
            {
                #[doc(hidden)]
                fn foreach<#generic_f>(
                    self,
                    f: &mut #generic_f
                ) where
                    #generic_f: #mapper #not_variadic_type_generics
                {
                }

                #[doc(hidden)]
                fn foreach_ref<#generic_f>(
                    &self,
                    f: &mut #generic_f
                ) where
                    #generic_f: #mapper_ref #not_variadic_type_generics
                {
                }

                #[doc(hidden)]
                fn foreach_mut<#generic_f>(
                    &mut self,
                    f: &mut #generic_f
                ) where
                    #generic_f: #mapper_mut #not_variadic_type_generics
                {
                }
            }

            impl #tuple_impl_generics
            #foreach #not_variadic_type_generics
            for ::tuplez::Tuple<#variadic_parameter_type, #generic_others>
            #tuple_where_clause
            {
                #[doc(hidden)]
                fn foreach<#generic_f>(
                    self,
                    f: &mut #generic_f
                ) where
                    #generic_f: #mapper #not_variadic_type_generics
                {
                    #mapper_fully_qualified::map::<#variadic_generic_type>(
                        f, self.0
                    );
                    #foreach_fully_qualified::foreach::<#generic_f>(
                        self.1, f
                    )
                }

                #[doc(hidden)]
                fn foreach_ref<#generic_f>(
                    &self,
                    f: &mut #generic_f
                ) where
                    #generic_f: #mapper_ref #not_variadic_type_generics
                {
                    #mapper_ref_fully_qualified::map_ref::<#variadic_generic_type>(
                        f, &self.0
                    );
                    #foreach_fully_qualified::foreach_ref::<#generic_f>(
                        &self.1, f
                    )
                }

                #[doc(hidden)]
                fn foreach_mut<#generic_f>(
                    &mut self,
                    f: &mut #generic_f
                ) where
                    #generic_f: #mapper_mut #not_variadic_type_generics
                {
                    #mapper_mut_fully_qualified::map_mut::<#variadic_generic_type>(
                        f, &mut self.0
                    );
                    #foreach_fully_qualified::foreach_mut::<#generic_f>(
                        &mut self.1, f
                    )
                }
            }
        };

        let variadic_bound = parse_quote! {
            #variadic_generic_type: #foreach #not_variadic_type_generics
        };

        (
            Self {
                mapper,
                mapper_ref,
                mapper_mut,
                foreach,
                impls,
            },
            variadic_bound,
        )
    }
}

impl syn::parse::Parse for CaptureVariableBind {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let variable = input.parse()?;
        let _: Token![=] = input.parse()?;
        let value = input.parse()?;
        Ok(Self { variable, value })
    }
}

impl syn::parse::Parse for CaptureVariableBindList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut list = vec![];
        while !input.is_empty() {
            list.push(input.parse()?);
            if !input.is_empty() {
                let _: Token![,] = input.parse()?;
            }
        }
        Ok(Self(list))
    }
}

impl VariadicExpander<'_> {
    fn visit_expr_block_mut_inner(&mut self, i: &mut syn::ExprBlock) -> syn::Result<()> {
        fn search_va_bind(attrs: &[Attribute]) -> syn::Result<HashMap<String, Expr>> {
            let mut map = HashMap::new();
            attrs
                .iter()
                .filter(|attr| attr.meta.path().is_ident("va_bind"))
                .try_for_each(|expr| -> syn::Result<()> {
                    let Meta::List(meta_list) = &expr.meta else {
                        return Ok(());
                    };
                    let CaptureVariableBindList(bind_list) = syn::parse2(meta_list.tokens.clone())?;
                    for CaptureVariableBind { variable, value } in bind_list {
                        map.insert(variable.to_string(), value);
                    }
                    Ok(())
                })?;
            Ok(map)
        }

        let ExprBlock {
            attrs,
            label,
            block,
        } = i;
        let expand_attr = attrs.iter().find(|attr| {
            attr.meta.path().is_ident("va_expand")
                || attr.meta.path().is_ident("va_expand_ref")
                || attr.meta.path().is_ident("va_expand_mut")
        });
        let Some(expand_attr) = expand_attr else {
            syn::visit_mut::visit_expr_block_mut(self, i);
            return Ok(());
        };
        if label.is_some() {
            return Err(syn::Error::new(
                label.span(),
                "label of expanded block is not available to use",
            ));
        }
        let mut variable_bind_map = search_va_bind(attrs)?;
        let local_struct_name = proc_macro2::Ident::new(
            &format!("__MapperImpl{}", nanoid!(16, &ALPHABETS)),
            proc_macro2::Span::mixed_site(),
        );

        let struct_decl: ItemStruct;
        let original_types: Option<TokenStream2>;
        let unpack: Option<TokenStream2>;
        let instantiation: Expr;
        if let Meta::List(list) = &expand_attr.meta {
            let tokens = &list.tokens;
            let parse_as_signature: Signature =
                syn::parse2(quote_spanned!(tokens.span() => fn __capture(#tokens)))?;
            let variable_list = parse_as_signature.inputs;
            let variable_list = variable_list
                .iter()
                .map(|arg| match arg {
                    FnArg::Receiver(arg) => Ok((
                        arg.mutability.is_some(),
                        parse_quote!(self),
                        (*arg.ty).clone(),
                    )),
                    FnArg::Typed(arg) => {
                        let Pat::Ident(ident) = &*arg.pat else {
                            return Err(syn::Error::new(
                                arg.pat.span(),
                                "variable captures do not support patterns",
                            ));
                        };
                        if ident.by_ref.is_some() {
                            return Err(syn::Error::new(
                                ident.by_ref.span(),
                                "variables are always captured by `ref`",
                            ));
                        }
                        Ok((
                            ident.mutability.is_some(),
                            ident.ident.clone(),
                            (*arg.ty).clone(),
                        ))
                    }
                })
                .collect::<syn::Result<Vec<(bool, Ident, Type)>>>()?;
            let original_types_list = variable_list
                .iter()
                .map(|(mutability, _, ty)| {
                    if *mutability {
                        parse_quote!(&mut #ty)
                    } else {
                        parse_quote!(&#ty)
                    }
                })
                .collect::<Vec<Type>>();
            let fields_types = variable_list
                .iter()
                .map(|(_, ident, _)| {
                    syn::parse_str(&format!("__T{}{}", ident, nanoid!(16, &ALPHABETS))).unwrap()
                })
                .collect::<Vec<Type>>();
            let fields_defs = variable_list
                .iter()
                .zip(fields_types.iter())
                .map(|((_, ident, _), ty)| quote!(#ident: #ty))
                .collect::<Vec<TokenStream2>>();
            let fields_init = variable_list
                .iter()
                .map(
                    |(mutability, ident, _)| match variable_bind_map.remove(&ident.to_string()) {
                        Some(expr) => {
                            if *mutability {
                                quote!(#ident: &mut (#expr))
                            } else {
                                quote!(#ident: & (#expr))
                            }
                        }
                        None => {
                            if *mutability {
                                quote!(#ident: &mut #ident)
                            } else {
                                quote!(#ident: &#ident)
                            }
                        }
                    },
                )
                .collect::<Vec<TokenStream2>>();
            let unpack_stmts = variable_list
                .iter()
                .map(|(mutability, ident, _)| {
                    if *mutability {
                        parse_quote! {
                            let #ident = &mut *self. #ident;
                        }
                    } else {
                        parse_quote! {
                            let #ident = &*self. #ident;
                        }
                    }
                })
                .collect::<Vec<Stmt>>();

            struct_decl = parse_quote! {
                struct #local_struct_name <#(#fields_types),*> {
                     #(#fields_defs),*
                }
            };
            original_types = Some(quote!(<#(#original_types_list),*>));
            unpack = Some(quote!(#(#unpack_stmts)*));
            instantiation = parse_quote! {
                #local_struct_name::#original_types { #(#fields_init),* }
            };
        } else {
            struct_decl = parse_quote!(struct #local_struct_name;);
            original_types = None;
            unpack = None;
            instantiation = parse_quote!(#local_struct_name);
        };

        let variadic_parameter_ident = &self.parameter.parameter_ident;
        let variadic_parameter_expr: Expr;
        let (not_variadic_impl_generics, not_variadic_type_generics, not_variadic_where_clause) =
            self.parameter.not_variadic_generics.split_for_impl();
        let impl_mapper_name;
        let foreach = &self.local_traits.foreach;
        let impl_mapper_method;
        let impl_mapper_method_pat_type;
        let impl_foreach_method;
        if expand_attr.meta.path().is_ident("va_expand") {
            impl_mapper_name = &self.local_traits.mapper;
            impl_mapper_method = Ident::new("map", Span::mixed_site());
            impl_mapper_method_pat_type = &self.parameter.parameter_pat_type;
            impl_foreach_method = Ident::new("foreach", Span::mixed_site());
            variadic_parameter_expr = parse_quote!(#variadic_parameter_ident);
        } else if expand_attr.meta.path().is_ident("va_expand_ref") {
            impl_mapper_name = &self.local_traits.mapper_ref;
            impl_mapper_method = Ident::new("map_ref", Span::mixed_site());
            impl_mapper_method_pat_type = &self.parameter.parameter_ref_pat_type;
            impl_foreach_method = Ident::new("foreach_ref", Span::mixed_site());
            variadic_parameter_expr = parse_quote!(&#variadic_parameter_ident);
        } else if expand_attr.meta.path().is_ident("va_expand_mut") {
            impl_mapper_name = &self.local_traits.mapper_mut;
            impl_mapper_method = Ident::new("map_mut", Span::mixed_site());
            impl_mapper_method_pat_type = &self.parameter.parameter_mut_pat_type;
            impl_foreach_method = Ident::new("foreach_mut", Span::mixed_site());
            variadic_parameter_expr = parse_quote!(&mut #variadic_parameter_ident);
        } else {
            unreachable!()
        };
        let variadic_generic_param = &self.parameter.generic_parameter;
        let variadic_generic_param: Generics = parse_quote!(<#variadic_generic_param>);
        let variadic_where_clause = &self.parameter.generic_where_clause;
        *block = parse_quote! {{
            #struct_decl

            impl #not_variadic_impl_generics
            #impl_mapper_name #not_variadic_type_generics
            for #local_struct_name #original_types
            #not_variadic_where_clause
            {
                fn #impl_mapper_method #variadic_generic_param (
                    &mut self, #impl_mapper_method_pat_type
                ) #variadic_where_clause
                {
                    #unpack
                    #block;
                }
            }

            #foreach::#impl_foreach_method (
                #variadic_parameter_expr,
                &mut #instantiation
            )
        }};
        attrs.clear();
        Ok(())
    }
}

impl VisitMut for VariadicExpander<'_> {
    fn visit_expr_block_mut(&mut self, i: &mut syn::ExprBlock) {
        if self.err.is_some() {
            return;
        }
        self.err = self.visit_expr_block_mut_inner(i).err();
    }

    // Skip all item
    fn visit_item_mut(&mut self, _: &mut Item) {}
}

#[proc_macro_attribute]
pub fn variadic(_: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as ItemFn);
    let parameter = match VariadicParameter::with_item_fn(&input) {
        Ok(v) => v,
        Err(e) => return e.into_compile_error().into(),
    };
    let (local_traits, variadic_bound) = LocalTraits::new(&parameter);

    input.attrs.push(parse_quote!(#[allow(private_bounds)]));
    input
        .sig
        .generics
        .where_clause
        .clone_from(&parameter.fn_bounds_not_variadic);
    input
        .sig
        .generics
        .make_where_clause()
        .predicates
        .push(variadic_bound);

    let mut expander = VariadicExpander {
        err: None,
        parameter: &parameter,
        local_traits: &local_traits,
    };
    expander.visit_block_mut(&mut input.block);
    if let Some(err) = expander.err {
        return err.into_compile_error().into();
    }

    let parameter_ident = &parameter.parameter_ident;
    let parameter_generic_ident = &parameter.generic_parameter.ident;
    input.sig.inputs.pop();
    input.sig.inputs.push_value(parse_quote! {
        #[allow(unused_mut)] mut #parameter_ident: #parameter_generic_ident
    });

    let local_trait_impls = local_traits.impls;

    quote! {
        #local_trait_impls
        #input
    }
    .into()
}

#[proc_macro_attribute]
pub fn variadic_impl(_: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as ItemImpl);
    let mut local_trait_impls = vec![];
    if let Err(e) = input
        .items
        .iter_mut()
        .try_for_each(|item| -> syn::Result<()> {
            let ImplItem::Fn(impl_item_fn) = item else {
                return Ok(());
            };
            let origin_len = impl_item_fn.attrs.len();
            impl_item_fn
                .attrs
                .retain(|attr| !attr.path().is_ident("variadic"));
            if impl_item_fn.attrs.len() == origin_len {
                return Ok(());
            }
            let parameter = VariadicParameter::with_impl_item_fn(impl_item_fn, &input.generics)?;
            let (local_traits, variadic_bound) = LocalTraits::new(&parameter);

            impl_item_fn
                .attrs
                .push(parse_quote!(#[allow(private_bounds)]));
            impl_item_fn
                .sig
                .generics
                .where_clause
                .clone_from(&parameter.fn_bounds_not_variadic);
            impl_item_fn
                .sig
                .generics
                .make_where_clause()
                .predicates
                .push(variadic_bound);

            let mut expander = VariadicExpander {
                err: None,
                parameter: &parameter,
                local_traits: &local_traits,
            };
            expander.visit_block_mut(&mut impl_item_fn.block);
            if let Some(err) = expander.err {
                return Err(err);
            }

            let parameter_ident = &parameter.parameter_ident;
            let parameter_generic_ident = &parameter.generic_parameter.ident;
            impl_item_fn.sig.inputs.pop();
            impl_item_fn.sig.inputs.push_value(parse_quote! {
                #[allow(unused_mut)] mut #parameter_ident: #parameter_generic_ident
            });

            local_trait_impls.push(local_traits.impls);
            Ok(())
        })
    {
        return e.into_compile_error().into();
    }

    quote! {
        #(#local_trait_impls)*
        #input
    }
    .into()
}
