DEBUG_DIR     := debug
GEN_DIR       := $(DEBUG_DIR)/gen
TEST_CASES_DIR := compiler-dev-test-cases/testcases

TARGET        := $(basename $(notdir $(TEST_FILE)))
ASM_OUT       := $(GEN_DIR)/$(TARGET).s
KOOPA_OUT     := $(GEN_DIR)/$(TARGET).koopa
BIN_OUT       := $(GEN_DIR)/$(TARGET)

# 创建 gen 目录（如果不存在）
$(shell mkdir -p $(GEN_DIR))

# 初始化子模块
init:
	@git submodule update --init --recursive

# 格式化代码
fmt:
	@cargo fmt

# 编译项目（cargo build）
build:
	@cargo build

# 清理并重新构建
rebuild:
	@cargo clean
	@cargo build

# 清理输出
clean:
	@rm -rf $(DEBUG_DIR)

# 编译为 Koopa IR
$(KOOPA_OUT): $(TEST_FILE)
	@cargo run -- -koopa $(TEST_FILE) -o $(KOOPA_OUT)

# 编译为 RISC-V 汇编
$(ASM_OUT): $(TEST_FILE)
	@cargo run -- -riscv $(TEST_FILE) -o $(ASM_OUT)

# 使用交叉编译工具链 gcc 进行编译
$(BIN_OUT): $(ASM_OUT)
	@riscv64-unknown-elf-gcc -static $< -o $(BIN_OUT)

# 运行 QEMU
run: $(BIN_OUT)
	@qemu-riscv64 $(BIN_OUT)

# 拷贝测试文件
prepare_test_file:
	@if [ -z "$(TEST_FILE)" ] || [ -z "$(LV)" ]; then \
		exit 1; \
	fi
	@mkdir -p $(DEBUG_DIR)
	@cp $(TEST_CASES_DIR)/lv$(LV)/$(TEST_FILE).c $(DEBUG_DIR)/$(TEST_FILE).c

# 测试用例：编译并运行
test: prepare_test_file
	@$(MAKE) TEST_FILE=$(DEBUG_DIR)/$(TEST_FILE).c run

.PHONY: init fmt build rebuild clean run test prepare_test_file