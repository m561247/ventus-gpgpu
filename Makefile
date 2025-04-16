SHELL := /bin/bash
# 定义 behavior 名称列表
BEHAVIORS = \
    Attention_NaiveTest \
    Attention_fp16888Test \
    Attention_fp16848Test \
    Attention_mix848Test \
    Linear_fp16_848Test \
    Linear_mix848Test \
    Linear_fp16_888Test \
    Linear_NaiveTest \
    Convolution_fp16_848Test \
    Convolution_mix848Test \
    Convolution_fp16_888Test \
    Convolution_NaiveTest \
    RNN_fp16_888Test \
    RNN_fp16_848Test \
    RNN_mix848Test \
    RNN_NaiveTest

init:
	git submodule update --init --recursive --progress

bump:
	git submodule foreach git stash
	git submodule update --remote
	git add dependencies

bsp:
	./mill -i mill.bsp.BSP/install

idea:
	./mill -i -j 0 mill.idea.GenIdea/idea

compile:
	./mill -i -j 0 __.compile

test:
	mkdir -p test_run_dir
	./mill -i ventus[6.4.0].tests.testOnly play.AdvancedTest 2>&1 | tee test_run_dir/AItest-MNIST_conv.log

verilog:
	./mill ventus[6.4.0].run

fpga-verilog:
	./mill ventus[6.4.0].runMain circt.stage.ChiselMain --module top.GPGPU_axi_adapter_top --target chirrtl --target-dir gen_fpga_verilog/
	cd gen_fpga_verilog/ &&  ~/.cache/llvm-firtool/1.62.0/bin/firtool --split-verilog --repl-seq-mem --repl-seq-mem-file=mem.conf -o . GPGPU_axi_adapter_top.fir
	./scripts/gen_sep_mem.sh ./scripts/vlsi_mem_gen gen_fpga_verilog/mem.conf gen_fpga_verilog/

clean:
	rm -rf out/ test_run_dir/ .idea/

clean-git:
	git clean -fd

