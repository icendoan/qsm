use std::env;

fn main()
{
    let mut d = env::current_dir().unwrap();
    d.push("src");
    d.push("c");
    println!("cargo:rustc-link-search={}", d.to_str().unwrap());
}
