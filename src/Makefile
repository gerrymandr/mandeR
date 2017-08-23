all:
	VARS_OLD := $(.VARIABLES)
	$(foreach v,                                        \
	$(filter-out $(VARS_OLD) VARS_OLD,$(.VARIABLES)),   \
	$(info $(v) = $($(v))))