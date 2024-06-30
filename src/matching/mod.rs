mod expr;
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

#[derive(Clone)]
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
    pub fn print(self) {
        match self {
            Match::ImplItem {
                item,
                file,
                impl_type,
                highlights,
            } => {
                println!(
                    "{}",
                    format_sig_with_highlights(&file, &item.sig, Some(&impl_type), &highlights)
                );
            }
            Match::Item {
                item,
                file,
                highlights,
            } => {
                println!(
                    "{}",
                    format_sig_with_highlights(&file, &item.sig, None, &highlights)
                );
            }
        }
    }
}

type Highlights = Vec<Span>;

fn add_highlights<S: Spanned>(item: S, highlights: &[Span]) -> String {
    // Extracts start and end of the span. I'm sure there's a proper way of doing this but alas, I
    // don't know what it is.
    fn span_bounds(span: &Span) -> (usize, usize) {
        let x = format!("{:?}", span);
        let x: Vec<_> = x.split(|c| c == '(' || c == '.' || c == ')').collect();
        (x[1].parse().unwrap(), x[3].parse().unwrap())
    }

    fn highlight_seq(depth: usize) -> String {
        let mut acc = "".to_string();
        //acc.push_str(&format!("X{}X\x1b[0m", depth));
        acc.push_str("\x1b[0m");
        if depth == 0 {
        } else if depth % 3 == 1 {
            acc.push_str("\x1b[1m\x1b[93m");
        } else if depth % 3 == 2 {
            acc.push_str("\x1b[1m\x1b[91m");
        } else {
            acc.push_str("\x1b[1m\x1b[94m");
        }
        acc
    }
    // We first convert the `highlights` spans to (usize, usize) tuples and adjust them to index
    // from the start of the item we're printing.
    let source = item.span().source_text().unwrap();
    let start = span_bounds(&item.span()).0;
    let mut highlights: Vec<_> = highlights
        .iter()
        .map(&span_bounds)
        .map(|(l, h)| (l - start, h - start))
        .collect();
    highlights.sort();

    let mut acc = "".to_string();
    let mut ends = vec![];
    let mut idx = 0;
    for (l, h) in highlights {
        while !ends.is_empty() && *ends.last().unwrap() <= l {
            let end = *ends.last().unwrap();
            acc.push_str(&source[idx..end]);
            idx = end;
            ends.pop();
            acc.push_str(&highlight_seq(ends.len()));
        }
        acc.push_str(&source[idx..l]);
        idx = l;
        ends.push(h);
        acc.push_str(&highlight_seq(ends.len()));
    }
    while !ends.is_empty() {
        let end = *ends.last().unwrap();
        if end <= idx {
            assert!(end == idx);
            acc.push_str(&highlight_seq(ends.len()));
            ends.pop();
        } else {
            acc.push_str(&highlight_seq(ends.len()));
            ends.pop();
            acc.push_str(&source[idx..end]);
            idx = end;
        }
    }
    acc.push_str(&highlight_seq(0));
    acc.push_str(&source[idx..]);
    acc
}

pub fn format_sig_with_highlights(
    file: &str,
    item: &syn::Signature,
    impl_type: Option<&syn::Type>,
    highlights: &[Span],
) -> String {
    // TODO: probably want to unindent the result?
    let impl_msg = match impl_type {
        Some(ty) => format!(" (in `impl {}`)", ty.span().source_text().unwrap()),
        None => "".to_string(),
    };

    format!(
        "\x1b[32m\x1b[1m{}:{}:{}\x1b[0m\n{}",
        file,
        item.span().start().line,
        impl_msg,
        add_highlights(item, highlights)
    )
}
