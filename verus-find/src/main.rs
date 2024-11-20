use clap::Parser;
use syn_verus as syn;

pub mod matching;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    #[clap(flatten)]
    ag_file: ArgGroupFile,
    #[clap(flatten)]
    ag_query: ArgGroupQuery,
}

#[derive(Parser, Debug)]
#[group(multiple = true)]
struct ArgGroupQuery {
    #[arg(
        long,
        value_name = "expr",
        help = "Find expression in requires or ensures"
    )]
    reqens: Option<String>,
    #[arg(short, long, value_name = "expr", help = "Find expression in requires")]
    req: Option<String>,
    #[arg(short, long, value_name = "expr", help = "Find expression in ensures")]
    ens: Option<String>,
    #[arg(short, long, value_name = "sig", help = "Find signature pattern")]
    sig: Option<String>,
    #[arg(short, long)]
    path: bool,
    //#[arg(short, long, value_name = "expr", help = "Expression to find in function body")]
    //body: Option<String>,
}

#[derive(Parser, Debug)]
#[group(required = true, multiple = false)]
struct ArgGroupFile {
    #[arg(
        short,
        long,
        value_name = "dependency file",
        help = "Dependency file for project to search in",
        conflicts_with("file")
    )]
    deps_file: Option<String>,
    #[arg(
        short,
        long,
        value_name = "verus file",
        help = "Verus file to search in"
    )]
    file: Option<String>,
}

fn main() {
    let args = Args::parse();
    let reqens: Option<syn::Expr> = args.ag_query.reqens.map(|expr| {
        syn::parse_str(&expr)
            .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", expr))
    });
    let req: Option<syn::Expr> = args.ag_query.req.map(|expr| {
        syn::parse_str(&expr)
            .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", expr))
    });
    let ens: Option<syn::Expr> = args.ag_query.ens.map(|expr| {
        syn::parse_str(&expr)
            .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", expr))
    });
    let body = None;
    //let body: Option<syn::Expr> = args.body.map(|_expr| {
    //    panic!("--body is not yet supported");
    //    //syn::parse_str(&expr).unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", expr))
    //});
    let sig: Option<syn::Signature> = args.ag_query.sig.map(|expr| {
        syn::parse_str(&expr)
            .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", expr))
    });
    let query = matching::Query::new(reqens, req, ens, body, sig);

    let (count, skipped) = if let Some(file) = args.ag_file.file {
        process_file(std::path::Path::new(&file), &query, args.ag_query.path)
    } else {
        let (root_path, files) =
            get_dependencies(std::path::Path::new(&args.ag_file.deps_file.unwrap()))
                .expect("Failed to get dependencies from deps file");
        let mut count = 0;
        let mut skipped = 0;
        for file in files {
            let (c, s) = process_file(&root_path.join(file), &query, args.ag_query.path);
            count += c;
            skipped += s;
        }
        (count, skipped)
    };
    println!("Found {} matches", count);
    if skipped > 0 {
        println!("Warning: Skipped {} matches", skipped);
    }
}

fn process_file(
    input_path: &std::path::Path,
    query: &matching::Query,
    print_path: bool,
) -> (usize, usize) {
    //println!("Processing {:?}", input_path);
    let file_content = std::fs::read_to_string(input_path)
        .map_err(|e| format!("cannot read {} ({})", input_path.display(), e))
        .unwrap();
    let file = syn_verus::parse_file(&file_content)
        .map_err(|e| {
            dbg!(&e.span().start(), &e.span().end());
            format!("failed to parse file {}: {}", input_path.display(), e)
        })
        .unwrap();
    let matches = matching::get_matches_file(&file, query, input_path.to_str().unwrap());
    let count = matches.len();
    let skipped = if print_path {
        let count_printed = matches
            .into_iter()
            .map(|m| m.print_as_path())
            .filter(|b| *b)
            .collect::<Vec<_>>()
            .len();
        count - count_printed
    } else {
        matches.into_iter().for_each(|m| m.print());
        0
    };
    (count, skipped)
}

// Copied from line_count tool
// parse the .d file and returns a vector of files names required to generate the crate
fn get_dependencies(
    dep_file_path: &std::path::Path,
) -> Result<(std::path::PathBuf, Vec<std::path::PathBuf>), String> {
    use std::path::{Path, PathBuf};

    let file = std::fs::File::open(dep_file_path)
        .map_err(|x| format!("{}, dependency file name: {:?}", x, dep_file_path))?;
    let mut reader = std::io::BufReader::new(file);
    let mut dependencies = String::new();
    std::io::BufRead::read_line(&mut reader, &mut dependencies).map_err(|x| {
        format!(
            "Could not read the first line of the dependency file with message: {}",
            x
        )
    })?;
    let dep_file_folder = dep_file_path.parent().ok_or(format!(
        "invalid dependencies file path {}",
        dep_file_path.display()
    ))?;
    let result: Vec<_> = dependencies
        .split_whitespace()
        .skip(1)
        .map(|x| dep_file_folder.join(Path::new(x)))
        .collect();
    assert!(!result.is_empty());

    if result.len() == 1 {
        return Ok((PathBuf::new(), vec![result[0].clone()]));
    }

    let mut path_iters: Vec<_> = result.iter().map(|x| x.iter()).collect();
    let mut chomp_components = 0;
    loop {
        let common = path_iters
            .iter_mut()
            .map(|x| x.next())
            .reduce(|acc, x| acc.and_then(|a| if Some(a) == x { Some(a) } else { None }))
            .flatten();
        if common.is_some() {
            chomp_components += 1;
        } else {
            break;
        }
    }
    let result_chomped: Vec<PathBuf> = result
        .iter()
        .map(|p| PathBuf::from_iter(p.components().skip(chomp_components)))
        .collect();
    let chomped = PathBuf::from_iter(result[0].iter().take(chomp_components));

    Ok((chomped, result_chomped))
}
