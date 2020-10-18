
# USER DEFINED PARAMETERS
GHDL = /Users/canaknesil/seperate-programs/ghdl-0.35-mcode-macosx/bin/ghdl
GTK_WAVE = /Volumes/MacSSD/Applications/gtkwave.app/Contents/Resources/bin/gtkwave

TOP_ENTITY = tb_VideoComposer
STD = 02 # 87/93/93c/00/02/08  select vhdl 87/93/00/02/08 standard
OTHER_GHDL_ANALSYE_OPTIONS = --ieee=synopsys -v

SRC = ../src
WORK_DIR = ./lib
GEN = ./gen

SRCS =  $(SRC)/package_MicroAssemblyCode.vhd # order is important
SRCS += $(SRC)/ppm_file_handler.vhd 
SRCS += $(SRC)/ALU.vhd 
SRCS += $(SRC)/RegFile.vhd 
SRCS += $(SRC)/Selector.vhd 
SRCS += $(SRC)/Shifter.vhd 
SRCS += $(SRC)/data_path.vhd 
SRCS += $(SRC)/single_port_ram.vhd 
SRCS += $(SRC)/single_port_rom.vhd 
#SRCS += $(SRC)/tb_composer.vhd 
SRCS += $(SRC)/test.vhd 
SRCS += $(SRC)/video_composer.vhd 
SRCS += $(SRC)/video_composer_fsmd.vhd 
SRCS += $(SRC)/tb_VideoComposer.vhd 
#SRCS += $(SRC)/videoComposer_fpga.vhd


# PREDEFINED PARAMETERS
WORK_FILE_CREATED = $(WORK_DIR)/work_file_created
GHDL_ANALYSE_OPTIONS = --std=$(STD) --workdir=$(WORK_DIR) -frelaxed-rules $(OTHER_GHDL_ANALSYE_OPTIONS)
VCD = $(GEN)/$(TOP_ENTITY).vcd

.PHONY: sim wave clean
ALL: $(WORK_FILE_CREATED)

# WILDCARD SOURCE VHDL FILES
#SRCS = $(wildcard $(SRC)/*.vhd)

# ANALYSE AND ELABORATE SOURCE FILES AND GENERATE WORKING LIBRARY
$(WORK_FILE_CREATED): $(SRCS)
	@echo ""
	@echo "====== ANALYSING ======"
	@echo ""

	mkdir -p $(WORK_DIR)
	$(GHDL) -a $(GHDL_ANALYSE_OPTIONS) $(SRCS)
	touch $(WORK_FILE_CREATED)

	@echo ""
	@echo "====== ELABORATING ======"
	@echo ""

	$(GHDL) -e $(GHDL_ANALYSE_OPTIONS) $(TOP_ENTITY)
	

# SIMULATE AND CREATE WAVEFORM FILE FOR GTKWAVE
$(VCD): $(WORK_FILE_CREATED)
	@echo ""
	@echo "====== SIMULATING AND CREATING WAVEFORM FILE ======"
	@echo ""

	mkdir -p gen
	$(GHDL) -r $(GHDL_ANALYSE_OPTIONS) $(TOP_ENTITY) --vcd=$(VCD)

# SIMULATE WITHOUT CREATING WAVEFORM FILE
sim: $(WORK_FILE_CREATED)

	@echo ""
	@echo "====== SIMULATING ======"
	@echo ""

	$(GHDL) -r $(GHDL_ANALYSE_OPTIONS) $(TOP_ENTITY)

# SIMULATE WITH GTKWAVE
wave: $(VCD)

	@echo ""
	@echo "====== OPENING GTK WAVE ======"
	@echo ""

	$(GTK_WAVE) $(VCD)

# REMOVE ALL CREATED FILES AND DIRECTORIES
clean: 
	rm -rf $(WORK_DIR) $(GEN)