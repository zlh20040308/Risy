DEBUG_DIR=debug

# 初始化子模块
init:
	@git submodule update --init --recursive

# 格式化代码
fmt:
	@cargo fmt

# 编译项目
build:
	@cargo build

# 清理并重新构建
rebuild:
	@cargo clean
	@cargo build

# 清理输出目录
clean:
	@rm -rf $(DEBUG_DIR)/**.c
	@rm -rf $(DEBUG_DIR)/**.s
	@rm -rf $(DEBUG_DIR)/gen/*

.PHONY: init fmt build rebuild clean