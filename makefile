# Choose the compiler and set compiler options

CPPCOMP  = g++
CPPOPTS  = -O3 

FCOMP    = gfortran
FOPTS    = -w  -cpp  -Ofast 
LDOPT    = -lfftw3 

JULIA    = julia
PYTHON   = python

FC     = $(FCOMP)
FFLAGS = $(FOPTS)
export FC FFLAGS

# Set the list of programs to compile

PROGRAMS = test_chebyshev test_idecomp test_jacobi_asym test_jacobi_taylor \
           test_jacobi_phase test_jacobi_quad test_jacobi_exp test_jacobi_transform


# Compile all of the test programs and the library

all	        : clean $(PROGRAMS) 


libdfftpack.a           :
	cd dfftpack && $(MAKE) clean && $(MAKE) && cp libdfftpack.a ..


# List the dependencies for each module's test program

JACOBI_EXPER_FILES     = utils.o                       \
                         gspiv.o                       \
                         orthom.o                      \
                         amos.o                        \
	                 idecomp.o                     \
                         chebyshev.o                   \
                         jacobi_asym.o                 \
                         jacobi_taylor.o               \
                         jacobi_phase.o                \
                         jacobi_quad.o                 \
                         jacobi_exp.o                  \
                         jacobi_transform.o
#                         libdfftpack.a


JACOBI_TRANSFORM_FILES = utils.o                       \
                         gspiv.o                       \
                         orthom.o                      \
                         amos.o                        \
	                 idecomp.o                     \
                         chebyshev.o                   \
                         jacobi_asym.o                 \
                         jacobi_taylor.o               \
                         jacobi_phase.o                \
                         jacobi_quad.o                 \
                         jacobi_exp.o                  \
                         jacobi_transform.o            \
                         libdfftpack.a

JACOBI_EXP_FILES       = utils.o                       \
                         gspiv.o                       \
                         orthom.o                      \
                         amos.o                        \
	                 idecomp.o                     \
                         chebyshev.o                   \
                         jacobi_asym.o                 \
                         jacobi_taylor.o               \
                         jacobi_phase.o                \
                         jacobi_quad.o                 \
                         jacobi_exp.o


JACOBI_QUAD_FILES      = utils.o                       \
                         amos.o                        \
                         chebyshev.o                   \
                         jacobi_asym.o                 \
                         jacobi_taylor.o               \
                         jacobi_phase.o                \
                         jacobi_quad.o


JACOBI_PHASE_FILES     = utils.o                       \
                         amos.o                        \
                         chebyshev.o                   \
                         jacobi_asym.o                 \
                         jacobi_taylor.o               \
                         jacobi_phase.o

JACOBI_TAYLOR_FILES    = utils.o                       \
                         chebyshev.o                   \
                         amos.o                        \
                         jacobi_asym.o                 \
                         jacobi_taylor.o

JACOBI_ASYM_FILES      = utils.o                       \
                         amos.o                        \
                         chebyshev.o                   \
                         jacobi_asym.o

IDECOMP_FILES          = utils.o                       \
                         orthom.o                      \
                         gspiv.o                       \
                         idecomp.o


CHEBYSHEV_FILES        = utils.o                       \
                         chebyshev.o





###################################################################################


run_experiment3:
	export JULIA_NUM_THREADS=1 && export OPENBLAS_NUM_THREADS=1 && $(JULIA) generate_transform.jl
	touch jacobi_experiment3.f90
	make jacobi_experiment3
	$(PYTHON) graph_transform1.py
	$(PYTHON) graph_transform2.py

run_experiment2	        :
	export JULIA_NUM_THREADS=1 && export OPENBLAS_NUM_THREADS=1 && $(JULIA) generate_quad.jl
	touch jacobi_experiment2.f90
	make  jacobi_experiment2
	$(PYTHON) graph_quad1.py
	$(PYTHON) graph_quad2.py

run_experiment1         :
	touch jacobi_experiment1.f90
	make  jacobi_experiment1
	$(PYTHON) graph_precomp.py
	$(PYTHON) graph_everrs.py


run_experiments: run_experiment1 run_experiment2 run_experiment3


jacobi_experiment3.o    : $(JACOBI_EXPER_FILES) jacobi_experiment3.f90
jacobi_experiment3      : $(JACOBI_EXPER_FILES) jacobi_experiment3.o

jacobi_experiment2.o    : $(JACOBI_EXPER_FILES) jacobi_experiment2.f90
jacobi_experiment2      : $(JACOBI_EXPER_FILES) jacobi_experiment2.o

jacobi_experiment1.o    : $(JACOBI_EXPER_FILES) jacobi_experiment1.f90
jacobi_experiment1      : $(JACOBI_EXPER_FILES) jacobi_experiment1.o


test_jacobi_transform.o : $(JACOBI_TRANSFORM_FILES) test_jacobi_transform.f90
test_jacobi_transform   : $(JACOBI_TRANSFORM_FILES) test_jacobi_transform.o

test_jacobi_exp.o       : $(JACOBI_EXP_FILES) test_jacobi_exp.f90
test_jacobi_exp         : $(JACOBI_EXP_FILES) test_jacobi_exp.o

test_jacobi_quad.o      : $(JACOBI_QUAD_FILES) test_jacobi_quad.f90
test_jacobi_quad        : $(JACOBI_QUAD_FILES) test_jacobi_quad.o

test_jacobi_phase.o     : $(JACOBI_PHASE_FILES) test_jacobi_phase.f90
test_jacobi_phase       : $(JACOBI_PHASE_FILES) test_jacobi_phase.o

test_jacobi_taylor.o    : $(JACOBI_TAYLOR_FILES) test_jacobi_taylor.f90
test_jacobi_taylor      : $(JACOBI_TAYLOR_FILES) test_jacobi_taylor.o

test_jacobi_asym.o      : $(JACOBI_ASYM_FILES) test_jacobi_asym.f90
test_jacobi_asym        : $(JACOBI_ASYM_FILES) test_jacobi_asym.o

test_idecomp.o          : $(IDECOMP_FILES) test_idecomp.f90
test_idecomp            : $(IDECOMP_FILES) test_idecomp.o

test_chebyshev.o        : $(CHEBYSHEV_FILES) test_chebyshev.f90
test_chebyshev          : $(CHEBYSHEV_FILES) test_chebyshev.o


# Setup the general compilation rules

%		: %.o
	$(FCOMP) $(FOPTS) -o $@ $^ $(LDOPT)
	@echo  
	@echo 
	@echo "---------[ $@     ]--------------------------------------------"
	@echo 
	@./$@
	@echo 
	@echo "--------------------------------------------------------------------------"
	@echo 

%.o		: %.f90
	$(FCOMP) -c $(FOPTS)  $<

%.o		: %.f
	$(FCOMP) -c $(FOPTS)  $<

%.o		: %.cpp
	$(CPPCOMP) -c $(CPPOPTS)  $<

.PHONY: clean

clean:
	rm -f *.o *.mod *~ fort.* a.out *.a
	rm -f $(PROGRAMS)
	rm -f *.dat
	rm -f *.py
	rm -f *.png
	rm -f table*.tex
	rm -f jacobi_experiment?



