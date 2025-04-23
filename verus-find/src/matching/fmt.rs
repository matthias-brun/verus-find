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

#[derive(Clone, Debug, Hash)]
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

fn add_highlights<S: Spanned>(item: S, highlights: &[(usize, usize)]) -> Vec<FmtToken> {
    // We first convert the `highlights` spans to (usize, usize) tuples and adjust them to index
    // from the start of the item we're printing.
    let source = item.span().source_text().unwrap();
    let start = item.span_bounds().0;
    let mut highlights: Vec<_> = highlights
        .iter()
        .map(|(l, h)| (l - start, h - start))
        .collect();
    // Sort by first element first; if equal, greater second element comes first.
    // Matches the nesting structure, e.g. we highlight (1, 10) and inside that highlight we have a
    // nested highlight (1,5). The outer highlight should come first.
    highlights.sort_by(|(l1, h1), (l2, h2)| l1.cmp(l2).then(h2.cmp(h1)));

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

pub fn format_location_line(file: &str, span: &Span, impl_type: Option<&syn::Type>) -> String {
    let impl_msg = match impl_type {
        Some(ty) => format!(" (in `impl {}`)", ty.span().source_text().unwrap()),
        None => "".to_string(),
    };

    format!("{}:{}:{}", file, span.start().line, impl_msg)
}

/// This function highlights the source span of the given signature with the given highlights.
/// The result is a vector of `FmtToken`s to separate the text from the highlighting. (Necessary to
/// prevent injection when generating HTML output.)
pub fn format_span_with_highlights<S: Spanned>(
    item: S,
    highlights: &[(usize, usize)],
) -> Vec<FmtToken> {
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
