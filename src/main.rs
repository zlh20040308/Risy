use clap::Parser;
use lalrpop_util::lalrpop_mod;

use std::fs;
use std::io::Result;

mod asm_gen;
mod ast;
mod context;
mod ir_gen;

use asm_gen::GenerateAsm;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    sysy
);

/// 编译器命令行参数
#[derive(Parser, Debug)]
#[command(author, version, about, disable_help_subcommand = true)]
struct Args {
    /// 模式，比如 -riscv 或 -koopa（注意，这里保留 `-`）
    #[arg(value_name = "MODE", allow_hyphen_values = true)]
    mode: String,

    /// 输入文件路径
    #[arg(value_name = "INPUT")]
    input: String,

    /// 输出文件路径
    #[arg(short = 'o', long = "output", value_name = "OUTPUT")]
    output: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let source_code = fs::read_to_string(&args.input)?;

    let ast = sysy::CompUnitParser::new()
        .parse(&source_code)
        .expect("Parse error");

    let json_str = serde_json::to_string_pretty(&ast).unwrap();
    fs::write("ast.json", json_str).expect("Failed to write to ast.json");

    let ir = ast.to_ir();

    match args.mode.as_str() {
        "-koopa" => {
            fs::write(&args.output, format!("{}", ir))?;
        }
        "-riscv" => {
            fs::write("ir.koopa", ir.clone()).expect("Failed to write to ir.koopa");
            let driver = koopa::front::Driver::from(ir.clone());
            let program = driver
                .generate_program()
                .expect("Failed to generate program");
            let asm = program.generate();
            fs::write(&args.output, asm)?;
        }
        _ => {
            eprintln!("Unknown mode: {}", args.mode);
        }
    }

    Ok(())
}
