use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=grammar/CreditCardDSL.g4");

    let out_dir = env::var("OUT_DIR").unwrap();
    let grammar_dir = PathBuf::from("grammar");
    let grammar_file = grammar_dir.join("CreditCardDSL.g4");

    // Check if ANTLR4 jar is available
    let antlr_jar = env::var("ANTLR4_JAR")
        .unwrap_or_else(|_| "antlr-4.13.1-complete.jar".to_string());

    println!("cargo:warning=Using ANTLR4 jar: {}", antlr_jar);

    // Generate parser using ANTLR4
    let status = Command::new("java")
        .args(&[
            "-jar",
            &antlr_jar,
            "-Dlanguage=Rust",
            "-o",
            &out_dir,
            "-visitor",
            "-no-listener",
            grammar_file.to_str().unwrap(),
        ])
        .status()
        .expect("Failed to execute ANTLR4. Make sure Java and ANTLR4 jar are installed.");

    if !status.success() {
        panic!("ANTLR4 parser generation failed");
    }

    println!("cargo:warning=ANTLR4 parser generated successfully in {}", out_dir);
}
