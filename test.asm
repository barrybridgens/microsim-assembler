	# Test file for microsim-assembler

	ORG 0x200

	ORG $210
	
start:	
	LDA_I	27
	JMP	start
	
