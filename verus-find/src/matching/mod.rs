mod expr;
pub mod fmt;
pub mod other;
#[cfg(test)]
mod test;

use expr::*;
use other::*;

use proc_macro2::Span;
use syn::spanned::Spanned;
use verus_syn as syn;

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

type Highlights = Vec<(usize, usize)>;

#[derive(Debug, Clone)]
pub struct Query {
    /// Pattern to find in requires or ensures
    reqens: Option<syn::Expr>,
    /// Pattern to find in requires
    req: Option<syn::Expr>,
    /// Pattern to find in ensures
    ens: Option<syn::Expr>,
    /// TODO: Pattern to find in function body
    _body: Option<syn::Expr>,
    /// Pattern to match signature against
    sig: Option<syn::Signature>,
}

impl Query {
    pub fn empty() -> Self {
        Query::new(None, None, None, None, None)
    }

    pub fn set_reqens(&mut self, reqens: Option<syn::Expr>) {
        self.reqens = reqens;
    }

    pub fn set_sig(&mut self, sig: Option<syn::Signature>) {
        self.sig = sig;
    }

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
        item: Box<syn::ImplItemFn>,
        file: String,
        impl_type: Box<syn::Type>,
        highlights: Vec<(usize, usize)>,
    },
    Item {
        item: Box<syn::ItemFn>,
        file: String,
        highlights: Vec<(usize, usize)>,
    },
    AssumeSpec {
        item: Box<syn::AssumeSpecification>,
        file: String,
        highlights: Vec<(usize, usize)>,
    },
}

impl Match {
    pub fn file(&self) -> &str {
        match self {
            Match::ImplItem { file, .. } => file,
            Match::Item { file, .. } => file,
            Match::AssumeSpec { file, .. } => file,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Match::ImplItem { item, .. } => item.sig.span(),
            Match::Item { item, .. } => item.sig.span(),
            Match::AssumeSpec { item, .. } => item.span(),
        }
    }

    pub fn impl_type(&self) -> Option<&syn::Type> {
        match self {
            Match::ImplItem { impl_type, .. } => Some(impl_type),
            Match::Item { .. } => None,
            Match::AssumeSpec { .. } => None,
        }
    }

    pub fn highlights(&self) -> &Vec<(usize, usize)> {
        match self {
            Match::ImplItem { highlights, .. } => highlights,
            Match::Item { highlights, .. } => highlights,
            Match::AssumeSpec { highlights, .. } => highlights,
        }
    }

    pub fn print(&self) {
        println!("{}", self.as_terminal_string());
    }

    pub fn as_fmt_tokens(&self) -> Vec<fmt::FmtToken> {
        fmt::format_span_with_highlights(self.span(), self.highlights())
    }

    pub fn as_terminal_string(&self) -> String {
        format!(
            "\x1b[32m\x1b[1m{}\x1b[0m\n{}",
            fmt::format_location_line(self.file(), &self.span(), self.impl_type()),
            fmt::fmt_tokens_to_terminal_string(fmt::format_span_with_highlights(
                self.span(),
                self.highlights()
            ))
        )
    }

    pub fn format_location_line(&self) -> String {
        fmt::format_location_line(self.file(), &self.span(), self.impl_type())
    }

    pub fn to_processed_match(&self) -> ProcessedMatch {
        ProcessedMatch {
            file: self.file().to_string(),
            line: self.span().start().line,
            loc_line: self.format_location_line(),
            source_fmt_tokens: self.as_fmt_tokens(),
        }
    }
}

pub trait SpanBounds {
    fn span_bounds(&self) -> (usize, usize);
}

impl<S: Spanned> SpanBounds for S {
    // Extracts start and end of the span. I'm sure there's a proper way of doing this but alas, I
    // don't know what it is.
    fn span_bounds(&self) -> (usize, usize) {
        let x = format!("{:?}", self.span());
        let x: Vec<_> = x.split(['(', '.', ')']).collect();
        (x[1].parse().unwrap(), x[3].parse().unwrap())
    }
}

pub fn get_matches_file(syn_file: &syn::File, query: &Query, file: &str) -> Vec<Match> {
    syn_file
        .items
        .iter()
        .flat_map(|item| other::get_matches_item(item, query, file))
        .collect()
}

#[derive(Clone, Hash)]
pub struct ProcessedMatch {
    pub file: String,
    pub line: usize,
    pub loc_line: String,
    pub source_fmt_tokens: Vec<fmt::FmtToken>,
}

impl ProcessedMatch {
    pub fn hash_with_default(&self) -> u64 {
        use std::hash::*;
        let mut s = DefaultHasher::new();
        self.hash(&mut s);
        s.finish()
    }
}
