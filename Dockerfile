FROM ubuntu:16.04
MAINTAINER Kavitha Srinivas <kavitha@rivetlabs.io>
RUN apt-get -qq update && apt-get install -y software-properties-common python-software-properties && \
    add-apt-repository main && \
    add-apt-repository restricted && \
    add-apt-repository universe && \
    add-apt-repository multiverse && \
    apt-get -qq update 

# install libs needed for racket
RUN apt-get install -y libcairo2 libpango1.0-0 libjpeg8

# install git, binutils, g++ make python for z3 compilation
RUN apt-get install -y wget binutils git g++ make python

# install racket
RUN wget --output-document=racket-install.sh -q https://mirror.racket-lang.org/installers/6.11/racket-6.11-x86_64-linux.sh && \
    echo "yes\n1\n" | /bin/bash racket-install.sh && \
        rm racket-install.sh
RUN raco setup
	
# install z3
RUN git clone https://github.com/Z3Prover/z3.git
RUN cd z3 && python scripts/mk_make.py && cd build && make && make install
RUN git clone https://github.com/Quetzal-RDF/rosette.git && cd /rosette && mkdir bin && cp /z3/build/z3 bin/
RUN cd /rosette && raco pkg install
RUN git clone https://github.com/Quetzal-RDF/synthesis
EXPOSE 8000
CMD ["racket", "-l", "errortrace", "-t", "/synthesis/webserver.rkt"]

