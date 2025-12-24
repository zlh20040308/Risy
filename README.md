# Risy  

一个玩具编译器，支持将`SysY`编译为`Koopa IR`或`RISC-V`汇编（未实现数组，也许未来会补上？）。  
BTW，有很多地方写的不好，比如`lalrpop`解析文件，为了让`LR(1)`算法开心作者不得不采用某些很冗余的做法。  
但无论如何，这是作者窥视编译器设计的一次尝试，在这点上作者自认为已经够了。  

## 🚀 使用方法

```shell
# 生成 Koopa IR
cargo run -- -koopa <输入文件路径> -o <输出文件路径>

# 生成 RISC-V 汇编
cargo run -- -riscv <输入文件路径> -o <输出文件路径>
```

## 📚 参考资料

- [北大编译实践在线文档](https://pku-minic.github.io/online-doc/#/)
- [空悬 else 问题 - 维基百科](https://en.wikipedia.org/wiki/Dangling_else)
- [Canonical LR parser - 维基百科](https://en.wikipedia.org/wiki/Canonical_LR_parser)

## 🛡️ 许可证

本项目采用 MulanPSL-2.0 许可证。查看 [LICENSE](LICENSE) 文件获取更多信息

## 🙏 致谢

感谢 Rust 社区提供优秀的开发工具和库！