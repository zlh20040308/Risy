use clap::Parser;
use env_logger::Builder;
use lalrpop_util::lalrpop_mod;
use log::Level;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use std::fs;
use std::io::Result;
use std::io::Write;

mod asmgen;
mod ast;

use asmgen::GenerateAsm;

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
    Builder::from_default_env()
        .format(|_buf, record| {
            let level = record.level();
            let mut stdout = StandardStream::stdout(ColorChoice::Always);

            // 设置颜色
            let color = match level {
                Level::Error => Color::Red,
                Level::Warn => Color::Yellow,
                Level::Info => Color::Green,
                Level::Debug => Color::Blue,
                Level::Trace => Color::Magenta,
            };

            stdout
                .set_color(ColorSpec::new().set_fg(Some(color)))
                .unwrap();

            // 输出日志
            writeln!(
                stdout,
                "{}: {}",
                level, // 带颜色的日志等级
                record.args()
            )
            .unwrap();

            // 重置颜色
            stdout.reset().unwrap();

            // 返回 Ok(())，确保返回类型为 Result<(), std::io::Error>
            Ok(())
        })
        .init();

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
