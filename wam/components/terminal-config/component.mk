COMPONENT_OWNBUILDTARGET := true
#COMPONENT_OWNCLEANTARGET := true

COMPONENT_EXTRA_CLEAN := config.pl

COMPONENT_ADD_LDFLAGS :=

build:
	echo ":- module(config, [config/2]). \
	config(register_count, "$(CONFIG_VM_REGISTER_COUNT)"). \
	config(functor_size, "$(CONFIG_VM_FUNCTOR_SIZE)"). \
	config(arity_size, "$(CONFIG_VM_ARITY_SIZE)"). \
	config(constant_size, "$(CONFIG_VM_CONSTANT_SIZE)"). \
	config(integer_size, "$(CONFIG_VM_INTEGER_SIZE)"). \
	" > config.pl
