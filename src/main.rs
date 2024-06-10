use clap::Parser;
use syn_verus as syn;

mod matching;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[clap(flatten)]
    ag: ArgGroup,
    #[arg(short, long)]
    req: Option<String>,
    #[arg(short, long)]
    ens: Option<String>,
    #[arg(short, long)]
    sig: Option<String>,
    #[arg(short, long)]
    body: Option<String>,
}

#[derive(Parser, Debug)]
#[group(required = true, multiple = false)]
struct ArgGroup {
    #[arg(short, long, conflicts_with("file"))]
    deps_file: Option<String>,
    #[arg(short, long)]
    file: Option<String>,
}

fn main() {
    let args = Args::parse();
    let req: Option<syn::Expr> = args.req.map(|expr| {
        syn::parse_str(&expr).expect(&format!("Failed to parse \"{}\" into expression", expr))
    });
    let ens: Option<syn::Expr> = args.ens.map(|expr| {
        syn::parse_str(&expr).expect(&format!("Failed to parse \"{}\" into expression", expr))
    });
    let body: Option<syn::Expr> = args.body.map(|_expr| {
        panic!("--body is not yet supported");
        //syn::parse_str(&expr).expect(&format!("Failed to parse \"{}\" into expression", expr))
    });
    let sig: Option<syn::Signature> = args.sig.map(|expr| {
        syn::parse_str(&expr).expect(&format!("Failed to parse \"{}\" into expression", expr))
    });
    let query = matching::Query::new(req, ens, body, sig);

    if let Some(file) = args.ag.file {
        process_file(&std::path::Path::new(&file), &query);
    } else {
        let (root_path, files) =
            get_dependencies(&std::path::Path::new(&args.ag.deps_file.unwrap()))
                .expect("Failed to get dependencies from deps file");
        files
            .iter()
            .for_each(|f| process_file(&root_path.join(f), &query));
    }
}

fn process_file(input_path: &std::path::Path, query: &matching::Query) {
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
    matching::find_and_print_matches_in_items(
        file.items.into_iter(),
        input_path.to_str().unwrap(),
        query,
    );
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
    assert!(result.len() > 0);

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
