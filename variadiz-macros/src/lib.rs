use nanoid::nanoid;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, parse_quote};

const ALPHABETS: [char; 62] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '1', '2', '3', '4', '5',
    '6', '7', '8', '9', '0',
];

fn is_variadic_pred(searched: &proc_macro2::Ident, tt: proc_macro2::TokenTree) -> bool {
    match tt {
        proc_macro2::TokenTree::Group(g) => g
            .stream()
            .into_iter()
            .any(|tt| is_variadic_pred(searched, tt)),
        proc_macro2::TokenTree::Ident(i) => &i == searched,
        _ => false,
    }
}

fn replace_stmts(
    stmts: Vec<syn::Stmt>,
    trait_mapper_name: &proc_macro2::Ident,
    trait_mapper_ref_name: &proc_macro2::Ident,
    trait_mapper_mut_name: &proc_macro2::Ident,
    trait_foreach_name: &proc_macro2::Ident,
    trait_impl_generics: &syn::ImplGenerics,
    trait_ty_generics: &syn::TypeGenerics,
    trait_where_clause: &Option<&syn::WhereClause>,
    variadic_ident: &proc_macro2::Ident,
    variadic_generic_param: &Option<syn::Generics>,
    variadic_where_clause: &Option<syn::WhereClause>,
    mapper_variadic_arg: &syn::PatType,
    mapper_variadic_arg_ref: &syn::PatType,
    mapper_variadic_arg_mut: &syn::PatType,
) -> Vec<syn::Stmt> {
    stmts
    .into_iter()
    .map(|stmt| match stmt {
        syn::Stmt::Expr(
            syn::Expr::Block(syn::ExprBlock {
                mut attrs,
                mut label,
                mut block,
            }),
            semi,
        ) => {
            let attr = attrs.into_iter().find(|attr| {
                attr.meta.path().is_ident("va_expand")
                    || attr.meta.path().is_ident("va_expand_ref")
                    || attr.meta.path().is_ident("va_expand_mut")
            });
            attrs = vec![];
            label = None;
            if let Some(attr) = attr {
                let struct_mapper_name = proc_macro2::Ident::new(
                    &format!("__Mapper{}", nanoid!(16, &ALPHABETS)),
                    proc_macro2::Span::call_site(),
                );
                let captures = if let syn::Meta::List(list) = &attr.meta {
                    let tokens = &list.tokens;
                    let variable_list: syn::FieldsNamed = parse_quote!({#tokens});
                    Some(variable_list)
                } else {
                    None
                };
                let (
                        struct_decl,
                        original_types,
                        unpack,
                        instance,
                    ):
                    (
                        syn::ItemStruct,
                        Option<proc_macro2::TokenStream>,
                        Option<syn::Stmt>,
                        syn::Expr,
                    ) = match captures {
                    Some(field_defs) => {
                        let mut field_defs = field_defs.named;
                        let mut original_types = vec![];
                        field_defs.iter_mut().for_each(|field| {
                            original_types.push(std::mem::replace(
                                &mut field.ty,
                                syn::parse_str(
                                    &format!(
                                        "__T{}{}",
                                        field.ident.as_ref().unwrap().to_string(),
                                        nanoid!(16, &ALPHABETS)
                                    )
                                ).unwrap()
                            ))
                        });
                        let fields_types = field_defs
                            .iter()
                            .map(|field| field.ty.clone())
                            .collect::<Vec<syn::Type>>();
                        let fields_idents = field_defs
                            .iter()
                            .map(|field| field.ident.clone().unwrap())
                            .collect::<Vec<syn::Ident>>();
                        (
                            parse_quote! {
                                struct #struct_mapper_name <#(#fields_types),*> { #field_defs }
                            },
                            Some(quote! { <#(#original_types),*> }),
                            Some(parse_quote! {
                                let Self { #(#fields_idents),* } = self;
                            }),
                            parse_quote! {
                                #struct_mapper_name { #(#fields_idents),* }
                            },
                        )
                    }
                    _ => (
                        parse_quote!(struct #struct_mapper_name;),
                        None,
                        None,
                        parse_quote! { #struct_mapper_name }
                    ),
                };

                if attr.meta.path().is_ident("va_expand") {
                    block = parse_quote! {
                        {
                            #struct_decl
                            impl #trait_impl_generics #trait_mapper_name #trait_ty_generics for
                            #struct_mapper_name #original_types #trait_where_clause
                            {
                                fn map #variadic_generic_param (&mut self, #mapper_variadic_arg)
                                #variadic_where_clause
                                {
                                    #unpack
                                    #block;
                                }
                            }
                            #trait_foreach_name::foreach(#variadic_ident, &mut #instance);
                        }
                    };
                } else if attr.meta.path().is_ident("va_expand_ref") {
                    block = parse_quote! {
                        {
                            #struct_decl
                            impl #trait_impl_generics #trait_mapper_ref_name #trait_ty_generics for
                            #struct_mapper_name #original_types #trait_where_clause
                            {
                                fn map_ref #variadic_generic_param (&mut self, #mapper_variadic_arg_ref)
                                #variadic_where_clause
                                {
                                    #unpack
                                    #block;
                                }
                            }
                            #trait_foreach_name::foreach_ref(&#variadic_ident, &mut #instance);
                        }
                    };
                } else if attr.meta.path().is_ident("va_expand_mut") {
                    block = parse_quote! {
                        {
                            #struct_decl
                            impl #trait_impl_generics #trait_mapper_mut_name #trait_ty_generics for
                            #struct_mapper_name #original_types #trait_where_clause
                            {
                                fn map_mut #variadic_generic_param (&mut self, #mapper_variadic_arg_mut)
                                #variadic_where_clause
                                {
                                    #unpack
                                    #block;
                                }
                            }
                            #trait_foreach_name::foreach_mut(&mut #variadic_ident, &mut #instance);
                        }
                    };
                } else {
                    unreachable!()
                }
            } else {
                block.stmts = replace_stmts(
                    block.stmts,
                    trait_mapper_name,
                    trait_mapper_ref_name,
                    trait_mapper_mut_name,
                    trait_foreach_name,
                    trait_impl_generics,
                    trait_ty_generics,
                    trait_where_clause,
                    variadic_ident,
                    variadic_generic_param,
                    variadic_where_clause,
                    mapper_variadic_arg,
                    mapper_variadic_arg_ref,
                    mapper_variadic_arg_mut,
                );
            }
            syn::Stmt::Expr(
                syn::Expr::Block(syn::ExprBlock {
                    attrs,
                    label,
                    block,
                }),
                semi,
            )
        }
        _ => stmt,
    })
    .collect()
}

#[proc_macro_attribute]
pub fn variadic(_: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as syn::ItemFn);
    if input.sig.inputs.is_empty() {
        return quote! {
           compile_error!("requires at least one parameter that is not a receiver");
        }
        .into();
    }
    let syn::FnArg::Typed(mut variadic_arg) = input.sig.inputs.pop().unwrap().into_value() else {
        return quote! {
           compile_error!("requires at least one parameter that is not a receiver");
        }
        .into();
    };
    let syn::Pat::Ident(variadic_ident) = &*variadic_arg.pat else {
        return quote! {
           compile_error!("variadic does not support pattern on parameter");
        }
        .into();
    };
    let variadic_ident = variadic_ident.ident.clone();
    let Some(syn::GenericParam::Type(variadic_generic)) = input.sig.generics.params.last() else {
        return quote! {
           compile_error!("requires generic type parameter for variadic");
        }
        .into();
    };
    let variadic_generic_type = variadic_generic.ident.clone();

    let trait_mapper_name = proc_macro2::Ident::new(
        &format!("__Mapper{}", nanoid!(16, &ALPHABETS)),
        proc_macro2::Span::call_site(),
    );
    let trait_mapper_ref_name = proc_macro2::Ident::new(
        &format!("__MapperRef{}", nanoid!(16, &ALPHABETS)),
        proc_macro2::Span::call_site(),
    );
    let trait_mapper_mut_name = proc_macro2::Ident::new(
        &format!("__MapperMut{}", nanoid!(16, &ALPHABETS)),
        proc_macro2::Span::call_site(),
    );
    let trait_foreach_name = proc_macro2::Ident::new(
        &format!("__Foreach{}", nanoid!(16, &ALPHABETS)),
        proc_macro2::Span::call_site(),
    );
    let for_each_generic_f = proc_macro2::Ident::new(
        &format!("__F{}", nanoid!(16, &ALPHABETS)),
        proc_macro2::Span::call_site(),
    );
    let for_each_generic_others = proc_macro2::Ident::new(
        &format!("__Others{}", nanoid!(16, &ALPHABETS)),
        proc_macro2::Span::call_site(),
    );

    let mapper_variadic_arg = variadic_arg.clone();
    let foreach_arg_type = variadic_arg.ty.clone();
    let mut mapper_variadic_arg_ref = mapper_variadic_arg.clone();
    mapper_variadic_arg_ref.ty = parse_quote!(&#foreach_arg_type);
    let mut mapper_variadic_arg_mut = mapper_variadic_arg.clone();
    mapper_variadic_arg_mut.ty = parse_quote!(&mut #foreach_arg_type);
    let mut generics_trait = input.sig.generics.clone();
    let mut generics_foreach_tuple = generics_trait.clone();
    let variadic_generic_param = generics_trait.params.pop();
    let variadic_generic_param: Option<syn::Generics> =
        variadic_generic_param.map(|param| parse_quote!(<#param>));
    let mut variadic_where_clause = generics_trait.where_clause.clone();
    if let Some(mut where_clause) = generics_trait.where_clause {
        let (variadic_related, non_variadic_related) =
            where_clause.predicates.into_iter().partition(|pred| {
                if let syn::WherePredicate::Type(type_pred) = pred {
                    type_pred
                        .to_token_stream()
                        .into_iter()
                        .any(|tt| is_variadic_pred(&variadic_generic_type, tt))
                } else {
                    false
                }
            });
        where_clause.predicates = non_variadic_related;
        generics_trait.where_clause = Some(where_clause);
        variadic_where_clause.as_mut().unwrap().predicates = variadic_related;
    }
    let (trait_impl_generics, trait_ty_generics, trait_where_clause) =
        generics_trait.split_for_impl();
    generics_foreach_tuple
        .params
        .push(parse_quote!(#for_each_generic_others));
    generics_foreach_tuple
        .make_where_clause()
        .predicates
        .push(parse_quote!(#for_each_generic_others: #trait_foreach_name #trait_ty_generics));
    let mapper_fully_qualified = if generics_trait.params.is_empty() {
        quote!( #trait_mapper_name)
    } else {
        quote!( #trait_mapper_name:: #trait_ty_generics )
    };
    let mapper_ref_fully_qualified = if generics_trait.params.is_empty() {
        quote!( #trait_mapper_ref_name)
    } else {
        quote!( #trait_mapper_ref_name:: #trait_ty_generics )
    };
    let mapper_mut_fully_qualified = if generics_trait.params.is_empty() {
        quote!( #trait_mapper_mut_name)
    } else {
        quote!( #trait_mapper_mut_name:: #trait_ty_generics )
    };
    let foreach_fully_qualified = if generics_trait.params.is_empty() {
        quote!( #trait_foreach_name)
    } else {
        quote!( #trait_foreach_name:: #trait_ty_generics )
    };
    let (tuple_impl_generics, _, tuple_where_clause) = generics_foreach_tuple.split_for_impl();

    let local_trait_impls = quote! {
        #[doc(hidden)]
        trait #trait_mapper_name #trait_impl_generics #trait_where_clause {
            #[doc(hidden)]
            #[allow(unused)]
            fn map #variadic_generic_param (&mut self, #mapper_variadic_arg)
            #variadic_where_clause {}
        }
        #[doc(hidden)]
        trait #trait_mapper_ref_name #trait_impl_generics #trait_where_clause {
            #[doc(hidden)]
            #[allow(unused)]
            fn map_ref #variadic_generic_param (&mut self, #mapper_variadic_arg_ref)
            #variadic_where_clause {}
        }
        #[doc(hidden)]
        trait #trait_mapper_mut_name #trait_impl_generics #trait_where_clause {
            #[doc(hidden)]
            #[allow(unused)]
            fn map_mut #variadic_generic_param (&mut self, #mapper_variadic_arg_mut)
            #variadic_where_clause {}
        }
        #[doc(hidden)]
        trait #trait_foreach_name #trait_impl_generics: ::tuplez::TupleLike
        #trait_where_clause
        {
            #[doc(hidden)]
            fn foreach<#for_each_generic_f>(self, f: &mut #for_each_generic_f)
            where
                #for_each_generic_f: #trait_mapper_name #trait_ty_generics;
            #[doc(hidden)]
            fn foreach_ref<#for_each_generic_f>(&self, f: &mut #for_each_generic_f)
            where
                #for_each_generic_f: #trait_mapper_ref_name #trait_ty_generics;
            #[doc(hidden)]
            fn foreach_mut<#for_each_generic_f>(&mut self, f: &mut #for_each_generic_f)
            where
                #for_each_generic_f: #trait_mapper_mut_name #trait_ty_generics;
        }
        impl #trait_impl_generics #trait_foreach_name #trait_ty_generics for
        ::tuplez::Unit #trait_where_clause {
            fn foreach<#for_each_generic_f>(self, _: &mut #for_each_generic_f)
            where
                #for_each_generic_f: #trait_mapper_name #trait_ty_generics
            {
            }
            fn foreach_ref<#for_each_generic_f>(&self, _: &mut #for_each_generic_f)
            where
                #for_each_generic_f: #trait_mapper_ref_name #trait_ty_generics
            {
            }
            fn foreach_mut<#for_each_generic_f>(&mut self, _: &mut #for_each_generic_f)
            where
                #for_each_generic_f: #trait_mapper_mut_name #trait_ty_generics
            {
            }
        }
        impl #tuple_impl_generics #trait_foreach_name #trait_ty_generics
        for ::tuplez::Tuple<#foreach_arg_type, #for_each_generic_others> #tuple_where_clause
        {
            fn foreach<#for_each_generic_f>(self, f: &mut #for_each_generic_f)
            where
                #for_each_generic_f: #trait_mapper_name #trait_ty_generics
            {
                #mapper_fully_qualified ::map::<#variadic_generic_type>(f, self.0);
                #foreach_fully_qualified ::foreach::<#for_each_generic_f>(self.1, f)
            }
            fn foreach_ref<#for_each_generic_f>(&self, f: &mut #for_each_generic_f)
            where
                #for_each_generic_f: #trait_mapper_ref_name #trait_ty_generics
            {
                #mapper_ref_fully_qualified ::map_ref::<#variadic_generic_type>(f, &self.0);
                #foreach_fully_qualified ::foreach_ref::<#for_each_generic_f>(&self.1, f)
            }
            fn foreach_mut<#for_each_generic_f>(&mut self, f: &mut #for_each_generic_f)
            where
                #for_each_generic_f: #trait_mapper_mut_name #trait_ty_generics
            {
                #mapper_mut_fully_qualified ::map_mut::<#variadic_generic_type>(f, &mut self.0);
                #foreach_fully_qualified ::foreach_mut::<#for_each_generic_f>(&mut self.1, f)
            }
        }
    };

    let Some(syn::GenericParam::Type(variadic_generic)) = input.sig.generics.params.last_mut()
    else {
        return quote! {
           compile_error!("requires generic type parameter for variadic");
        }
        .into();
    };
    let variadic_generic_type = variadic_generic.ident.clone();
    std::mem::take(&mut variadic_generic.bounds);
    variadic_arg.ty = Box::new(parse_quote!(#variadic_generic_type));
    variadic_arg.attrs.push(parse_quote!(#[allow(unused_mut)]));
    variadic_arg.pat = parse_quote!(mut #variadic_ident);
    input.sig.inputs.push(syn::FnArg::Typed(variadic_arg));

    input.sig.generics.where_clause = generics_trait.where_clause.clone();
    input
        .sig
        .generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(#variadic_generic_type: #trait_foreach_name #trait_ty_generics));

    input.block.stmts = replace_stmts(
        input.block.stmts,
        &trait_mapper_name,
        &trait_mapper_ref_name,
        &trait_mapper_mut_name,
        &trait_foreach_name,
        &trait_impl_generics,
        &trait_ty_generics,
        &trait_where_clause,
        &variadic_ident,
        &variadic_generic_param,
        &variadic_where_clause,
        &mapper_variadic_arg,
        &mapper_variadic_arg_ref,
        &mapper_variadic_arg_mut,
    );

    quote! {
        #local_trait_impls
        #input
    }
    .into()
}
