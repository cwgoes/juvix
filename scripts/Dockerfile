FROM fpco/stack-build:lts-13.30

RUN ln -s /usr/bin/g++-7 /usr/bin/g++

RUN git clone https://github.com/Z3Prover/z3.git && cd z3 && python scripts/mk_make.py && cd build && make && make install