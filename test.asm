	# Test file for microsim-assembler

	ORG 0x200
var1:
	
	ORG $210
	
start:	
	LDA_I	27

	ORG $220
	
	LDA_M	var1
	INC
	DEC
	OUT
	STA	var1
	JMP	start
	
