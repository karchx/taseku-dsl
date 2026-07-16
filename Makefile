.PHONY: compile_cbits 

compile_cbits:
	gcc -fPIC -shared cbits.c -o cbits.so
	mkdir -p cbits && mv cbits.so cbits

