# 定义测试文件路径
DEBUG_DIR = debug
GEN_DIR = $(DEBUG_DIR)/gen
TEST_FILE=$(DEBUG_DIR)/hello.c

$(shell mkdir -p $(GEN_DIR))


# 初始化子模块
init:
	@git submodule update --init --recursive

# 格式化代码
fmt:
	@cargo fmt

# 运行 cargo 命令进行编译并生成目标文件
riscv: $(TEST_FILE)
	@cargo run -- -riscv $(TEST_FILE) -o $(GEN_DIR)/hello.s

# 运行 cargo 命令进行编译并测试
koopa: $(TEST_FILE)
	@cargo run -- -koopa $(TEST_FILE) -o $(GEN_DIR)/hello.koopa

# 使用 QEMU 运行编译的 main.S 程序
qemu:
	riscv64-unknown-elf-gcc -static $(GEN_DIR)/hello.s -o $(GEN_DIR)/hello
	qemu-riscv64 $(GEN_DIR)/hello

# 清理并重新编译项目
rebuild:
	@cargo clean
	@cargo build

ref:
	@gcc $(TEST_FILE) -o $(GEN_DIR)/hello
	@./$(GEN_DIR)/hello

clean:
	rm -rf $(GEN_DIR)


# 默认目标
.PHONY: init run fmt test rebuild qemu