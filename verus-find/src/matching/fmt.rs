use super::*;

#[derive(Clone, Debug, Copy, std::hash::Hash, PartialEq, Eq)]
pub struct HLToken(pub usize);

impl HLToken {
    fn fmt_as_term_end(&self) -> &'static str {
        "\x1b[0m"
    }

    fn fmt_as_term_begin(&self) -> &'static str {
        if self.0 == 0 {
            ""
        } else if self.0 % 3 == 1 {
            "\x1b[1m\x1b[93m"
        } else if self.0 % 3 == 2 {
            "\x1b[1m\x1b[91m"
        } else {
            "\x1b[1m\x1b[94m"
        }
    }
}

#[derive(Clone, Debug)]
pub enum FmtToken {
    Newline,
    Text(String, HLToken),
}

impl FmtToken {
    fn as_terminal(&self) -> String {
        match self {
            FmtToken::Newline => "\n".to_string(),
            FmtToken::Text(s, c) => {
                format!("{}{}{}", c.fmt_as_term_begin(), s, c.fmt_as_term_end())
            }
        }
    }

    pub fn hash(&self) -> u64 {
        use std::hash::*;
        let mut s = DefaultHasher::new();
        match self {
            FmtToken::Newline => {
                "1".hash(&mut s);
            }
            FmtToken::Text(text, hlt) => {
                "2".hash(&mut s);
                text.hash(&mut s);
                hlt.hash(&mut s);
            }
        }
        s.finish()
    }
}

// Extracts start and end of the span. I'm sure there's a proper way of doing this but alas, I
// don't know what it is.
pub fn span_bounds(span: &Span) -> (usize, usize) {
    let x = format!("{:?}", span);
    let x: Vec<_> = x.split(|c| c == '(' || c == '.' || c == ')').collect();
    (x[1].parse().unwrap(), x[3].parse().unwrap())
}

fn add_highlights<S: Spanned>(item: S, highlights: &[Span]) -> Vec<FmtToken> {
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

    let mut tokens = vec![];
    let mut ends = vec![];
    let mut idx = 0;
    for (l, h) in highlights {
        while !ends.is_empty() && *ends.last().unwrap() <= l {
            let end = *ends.last().unwrap();
            tokens.push(FmtToken::Text(
                source[idx..end].to_string(),
                HLToken(ends.len()),
            ));
            idx = end;
            ends.pop();
        }
        tokens.push(FmtToken::Text(
            source[idx..l].to_string(),
            HLToken(ends.len()),
        ));
        idx = l;
        ends.push(h);
    }
    while !ends.is_empty() {
        let end = *ends.last().unwrap();
        if end <= idx {
            assert!(end == idx);
            ends.pop();
        } else {
            tokens.push(FmtToken::Text(
                source[idx..end].to_string(),
                HLToken(ends.len()),
            ));
            ends.pop();
            idx = end;
        }
    }
    assert!(ends.is_empty());
    tokens.push(FmtToken::Text(source[idx..].to_string(), HLToken(0)));
    tokens
}

pub fn format_location_line(
    file: &str,
    item: &syn::Signature,
    impl_type: Option<&syn::Type>,
) -> String {
    let impl_msg = match impl_type {
        Some(ty) => format!(" (in `impl {}`)", ty.span().source_text().unwrap()),
        None => "".to_string(),
    };

    format!("{}:{}:{}", file, item.span().start().line, impl_msg)
}

pub fn format_sig_with_highlights(item: &syn::Signature, highlights: &[Span]) -> Vec<FmtToken> {
    // TODO: probably want to unindent the result?
    let tokens = add_highlights(item, highlights);
    tokens
        .into_iter()
        .flat_map(|tok| {
            // Replace newlines in the text with newline tokens
            if let FmtToken::Text(s, d) = tok {
                s.lines()
                    .map(|l| FmtToken::Text(l.to_string(), d))
                    .zip(std::iter::repeat(FmtToken::Newline))
                    .flat_map(|(a, sep)| vec![sep, a])
                    .skip(1)
                    .collect()
            } else {
                vec![tok]
            }
        })
        .collect()
}

pub fn fmt_tokens_to_terminal_string(tokens: Vec<FmtToken>) -> String {
    tokens.into_iter().map(|tok| tok.as_terminal()).collect()
}
