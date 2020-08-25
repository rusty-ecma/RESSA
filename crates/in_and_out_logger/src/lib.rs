use proc_macro::TokenStream as StdStream;
use syn::ItemFn;

use proc_macro2::TokenStream;

#[proc_macro_attribute]
pub fn log_in_and_out(_attr: StdStream, body: StdStream) -> StdStream {
    let f: ItemFn = syn::parse_macro_input!(body);
    handle_f(f).into()
}

fn handle_f(f: ItemFn) -> TokenStream {
    let name = f.sig.ident.clone().to_string();
    let body = f.block.clone();
    let sig = f.sig.clone();
    let vis = f.vis.clone();
    quote::quote!{
        #vis #sig {
            println!("-> {}", #name);
            let mut f = move || #body;
            let ret = f();
            println!("<- {}", #name);
            ret
        }
    }
}