# This comments is three lines long due to the trailing slash\
This line should still be in the comment style\
As should this. But the following line should not be \\
NOT_A_COMMENT := 1

# 1) MAKEFLAGS is a special variable
MAKEFLAGS = -r

# conditional. The indent is by spaces.
ifeq ($(TARGET_ARCH),)
  TARGET_ARCH := 64
endif

# More complex conditionals involving a shell expression.
ifeq ($(shell test $(SOURCE_VERSION) -lt 2 ; echo $$?),0)
  MAGIC := bacon/$(SOURCE_VERSION)
else ifeq ($(shell test $(SOURCE_VERSION) -eq 2 ; echo $$?),0)
  MAGIC := toast/$(SOURCE_VERSION)
else
  MAGIC := waffles/$(SOURCE_VERSION)
endif

# Variable assignments of different forms.
EXTERNAL_PREFIX_DIR := ../build/linux$(TARGET_ARCH)/external

FOO = $_
FOO += $x

# The include in the following variable assignment should not be recognized as an include
# directive.

INCLUDE_DIRS := $(INCLUDE_DIRS) \
. \
$(EXTERNAL_PREFIX_DIR)/include \

# include test start
# However, the lines up to the 'include test end' should all be recognized as include 
# directives
include foo
-include baz
   include zup asd ggg # This should be parsed as a comment. 
   -include fub fab fiz

# The following line contains variable expansions inside the include directive. They 
# should be recognized
 include $(BACON) $(shell echo kittens) 
 
# The following include directive includes filename globs. I'd like these to be recognized
# as different from string literals
-include *.mk disco*bar

# include test end

ifndef $(BUILD_VARIANT)
  BUILD_VARIANT := debug
endif

BUILD_DIR_BASE := ../build/linux$(TARGET_ARCH)/$(BUILD_VARIANT)/
OBJECTS_DIR := $(BUILD_DIR_BASE)objects/
DEPENDS_DIR := $(BUILD_DIR_BASE)depends/
BIN_DIR := $(BUILD_DIR_BASE)bin/
EXE_PATH := $(BIN_DIR)$(EXE_NAME)

ifndef $(VERSION_SUFFIX)
  VERSION_SUFFIX := _dev_only
endif
VERSION_IDENTIFIER_BASE := $(shell date +'%Y%m%d')
VERSION_IDENTIFIER := $(VERSION_IDENTIFIER_BASE)$(VERSION_SUFFIX)

INC_FLAGS := $(addprefix -I,$(INCLUDE_DIRS)) 
CPPFLAGS := $(INC_FLAGS)
LIB_DIR_FLAGS := $(addprefix -L, $(LIB_DIRS))
CXXFLAGS := -pthread -ggdb
WARNING_FLAGS := -Werror -Wall 

ifeq ($(BUILD_VARIANT),release)
  CPPFLAGS += -DRELEASE_BUILD
  CXXFLAGS += -O3
endif

ifeq ($(BUILD_VARIANT),debug)
  CPPFLAGS += -DDEBUG_BUILD
  CXXFLAGS += -O0
endif

CPP_SOURCES := $(filter %.cpp, $(SOURCES))
CPP_OBJECTS := $(patsubst %.cpp, %.cpp.o, $(CPP_SOURCES))
OBJECTS := $(addprefix $(OBJECTS_DIR), $(CPP_OBJECTS))

DEPENDS_FILES := $(patsubst %,%.depends ,$(addprefix $(DEPENDS_DIR),$(SOURCES)))

STATIC_LIBS := $(STATIC_LIBS) \
$(MAGIC)/foo.a

DYNAMIC_LIBS_LINK_OPTS := $(addprefix -l,$(DYNAMIC_LIBS))

# Start of rules.
# The code in the rules is shell code and it's left up to the shell to how it should be processed. 
# From the perspective of the Makefile I think the shell code should be treated as an interpolated
# string.
.PHONY : all
all : $(DEPENDS_FILES) 
	@MAKEFILES='$(DEPENDS_FILES)' $(MAKE) all-minus-depends

.PHONY : all-minus-depends
all-minus-depends : $(EXE_PATH)

# Not a phony rule.
$(EXE_PATH) : $(BIN_DIR) $(EXTERNALS) $(OBJECTS)
	# Main compile rule.
	g++ -o $@ $(LIB_DIR_FLAGS) $(OBJECTS) \
		-lpthread \
        $(DYNAMIC_LIBS_LINK_OPTS) \
		$(STATIC_LIBS)

$(BIN_DIR) : $(BUILD_DIR_BASE)
	mkdir -p $(BIN_DIR)
	echo $$PWD

$(BUILD_DIR_BASE) : 
	@echo Creating build directory.
	mkdir -p "$(BUILD_DIR_BASE)"

$(DEPENDS_DIR) : 
	mkdir -p '$(DEPENDS_DIR)'

# Involving some GNU specific magic.
.SECONDEXPANSION:
$(DEPENDS_FILES) : $$(subst $(DEPENDS_DIR),,$$(dir $$@))$$(subst .depends,,$$(@F)) Makefile | $(EXTERNALS)
	@mkdir -p `dirname $@`
	cpp -MF '$@' -M $(CPPFLAGS) $<
	sed -i -e 's:.*\::$(subst $(DEPENDS_DIR),$(OBJECTS_DIR),$(subst .depends,.o,$@)) $@\::' $@

.SECONDEXPANSION:
%.cpp.o : $$(subst $(OBJECTS_DIR),,$$(dir $$@))$$*.cpp
	@echo Applying build rule for $@
	@mkdir -p `dirname $@`
	g++ -DVERSION_IDENTIFIER=\"$(VERSION_IDENTIFIER)\" -c -o $@ $(WARNING_FLAGS) $(CPPFLAGS) $(CXXFLAGS) $<

EXTERNAL_CLEAN_TARGETS := $(addsuffix -clean,$(EXTERNALS))

.PHONY : clean
clean : $(EXTERNAL_CLEAN_TARGETS)
	rm -rf $(OBJECTS)
	rm -rf $(EXE_PATH)

$(EXTERNALS) : $(EXTERNAL_PREFIX_DIR)
$(EXTERNAL_PREFIX_DIR) :
	@mkdir -p $(EXTERNAL_PREFIX_DIR)

# What follows makes no sense but is perfectly valid.
# The $$ should be recognized as a variable identifier and *not* as escaping $ as it is in a rule 
# command. 
$$=FOO
FOO:=$($$)

# Mixing brackets within a variable expansion.
$(shell echo ${shell echo $($$)})
