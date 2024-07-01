mod expr;
pub mod fmt;
pub mod other;
#[cfg(test)]
mod test;

use expr::*;
use other::*;

use proc_macro2::Span;
use syn::spanned::Spanned;
use syn_verus as syn;

macro_rules! yes {
    () => {
        Some(vec![])
    };
}
pub(crate) use yes;

macro_rules! no {
    () => {
        None
    };
}
pub(crate) use no;

macro_rules! yes_if {
    ($e1:expr) => {
        if $e1 {
            yes!()
        } else {
            None
        }
    };
    ($e1:expr, with_span: $e2:expr) => {
        if $e1 {
            Some(vec![$e2])
        } else {
            None
        }
    };
}
pub(crate) use yes_if;

macro_rules! and {
    () => (yes!());
    ($e:expr) => ($e);
    ($e1:expr $( , $es:expr )*) => (
        match $e1 {
            Some(hl1) => {
                match and!($( $es ), *) {
                    Some(hl2) => Some([hl1,hl2].concat()),
                    None      => None
                }
            },
            None => None
        })
}
pub(crate) use and;

macro_rules! or {
    () => (no!());
    ($e:expr) => ($e);
    ($e1:expr $( , $es:expr )*) => (
        match $e1 {
            Some(hl1) => {
                match or!($( $es ), *) {
                    Some(hl2) => Some([hl1,hl2].concat()),
                    None      => Some(hl1)
                }
            },
            None => or!($( $es ), *),
        })
}
pub(crate) use or;

// Alternatively could short-circuit the or:
// (i.e. highlight only the first match but do less extra work)
//macro_rules! or {
//    () => (no!());
//    ($e:expr) => ($e);
//    ($e1:expr $( , $es:expr )*) => (
//        match $e1 {
//            Some(hl1) => Some(hl1),
//            None => or!($( $es ), *),
//        })
//}

type Highlights = Vec<Span>;

pub struct Query {
    reqens: Option<syn::Expr>,
    req: Option<syn::Expr>,
    ens: Option<syn::Expr>,
    _body: Option<syn::Expr>,
    sig: Option<syn::Signature>,
}

impl Query {
    pub fn new(
        reqens: Option<syn::Expr>,
        req: Option<syn::Expr>,
        ens: Option<syn::Expr>,
        body: Option<syn::Expr>,
        sig: Option<syn::Signature>,
    ) -> Self {
        Query {
            reqens,
            req,
            ens,
            _body: body,
            sig,
        }
    }

    pub fn reqens(&self) -> Option<&syn::Expr> {
        self.reqens.as_ref()
    }

    pub fn req(&self) -> Option<&syn::Expr> {
        self.req.as_ref()
    }

    pub fn ens(&self) -> Option<&syn::Expr> {
        self.ens.as_ref()
    }

    #[allow(dead_code)]
    pub fn body(&self) -> Option<&syn::Expr> {
        self._body.as_ref()
    }

    pub fn sig(&self) -> Option<&syn::Signature> {
        self.sig.as_ref()
    }
}

#[derive(Clone, Debug)]
pub enum Match {
    ImplItem {
        item: syn::ImplItemMethod,
        file: String,
        impl_type: syn::Type,
        highlights: Vec<Span>,
    },
    Item {
        item: syn::ItemFn,
        file: String,
        highlights: Vec<Span>,
    },
}

impl Match {
    pub fn file(&self) -> &str {
        match self {
            Match::ImplItem { file, .. } => file,
            Match::Item { file, .. } => file,
        }
    }

    pub fn sig(&self) -> &syn::Signature {
        match self {
            Match::ImplItem { item, .. } => &item.sig,
            Match::Item { item, .. } => &item.sig,
        }
    }

    pub fn impl_type(&self) -> Option<&syn::Type> {
        match self {
            Match::ImplItem { impl_type, .. } => Some(impl_type),
            Match::Item { .. } => None,
        }
    }

    pub fn highlights(&self) -> &Vec<Span> {
        match self {
            Match::ImplItem { highlights, .. } => highlights,
            Match::Item { highlights, .. } => highlights,
        }
    }

    pub fn print(&self) {
        println!("{}", self.as_terminal_string());
    }

    pub fn as_fmt_tokens(&self) -> Vec<fmt::FmtToken> {
        fmt::format_sig_with_highlights(self.sig(), self.highlights())
    }

    pub fn as_terminal_string(&self) -> String {
        format!(
            "\x1b[32m\x1b[1m{}\x1b[0m\n{}",
            fmt::format_location_line(self.file(), self.sig(), self.impl_type()),
            fmt::fmt_tokens_to_terminal_string(fmt::format_sig_with_highlights(
                self.sig(),
                self.highlights()
            ))
        )
    }

    pub fn format_location_line(&self) -> String {
        fmt::format_location_line(self.file(), self.sig(), self.impl_type())
    }

    pub fn hash(&self) -> u64 {
        use std::hash::*;
        let mut s = DefaultHasher::new();
        self.file().hash(&mut s);
        self.sig().hash(&mut s);
        self.impl_type().hash(&mut s);
        self.highlights()
            .iter()
            .map(fmt::span_bounds)
            .collect::<Vec<_>>()
            .hash(&mut s);
        s.finish()
    }
}
