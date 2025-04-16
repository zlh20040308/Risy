import os
import sys
import subprocess

PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
os.chdir(PROJECT_ROOT)

DEBUG_DIR = "debug"
GEN_DIR = os.path.join(DEBUG_DIR, "gen")
TEST_CASES_DIR = "compiler-dev-test-cases/testcases"

def run_cmd(cmd, capture=False):
    result = subprocess.run(cmd, shell=True, capture_output=capture)
    return result.returncode

def main():
    # 获取 LV 参数（默认 lv1）
    lv = sys.argv[1] if len(sys.argv) > 1 else "lv1"
    lv_dir = os.path.join(TEST_CASES_DIR, lv)

    if not os.path.exists(lv_dir):
        print(f"[ERROR] 目录不存在：{lv_dir}")
        sys.exit(1)

    test_cases = sorted(f for f in os.listdir(lv_dir) if f.endswith(".c"))
    os.makedirs(GEN_DIR, exist_ok=True)
    os.makedirs(DEBUG_DIR, exist_ok=True)

    passed = 0

    for test_file in test_cases:
        test_name = os.path.splitext(test_file)[0]
        src_path = os.path.join(lv_dir, test_file)
        copy_path = os.path.join(DEBUG_DIR, test_file)
        asm_path = os.path.join(GEN_DIR, f"{test_name}.s")
        bin_path = os.path.join(GEN_DIR, f"{test_name}")
        host_path = os.path.join(DEBUG_DIR, "host")

        print(f"[TEST] {test_name}...", end=" ")

        os.system(f"cp {src_path} {copy_path}")

        ret = run_cmd(f"cargo run -- -riscv {copy_path} -o {asm_path}")
        if ret != 0:
            print("❌ compile failed")
            continue

        ret = run_cmd(f"riscv64-unknown-elf-gcc -static {asm_path} -o {bin_path}")
        if ret != 0:
            print("❌ riscv gcc failed")
            continue

        qemu_ret = run_cmd(f"qemu-riscv64 {bin_path}")
        run_cmd(f"gcc {copy_path} -o {host_path}")
        host_ret = run_cmd(f"./{host_path}")

        if qemu_ret == host_ret:
            print("✅")
            passed += 1
        else:
            print(f"❌ (qemu: {qemu_ret}, host: {host_ret})")

    print(f"\n[SUMMARY] Passed {passed}/{len(test_cases)}")

if __name__ == "__main__":
    main()
