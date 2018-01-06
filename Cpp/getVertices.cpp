#include "ppl.hh"
#include <sstream>
#include <fstream>
#include <string>

extern "C" {

using namespace Parma_Polyhedra_Library;

char* getVertices(char* constraintsFile){
    Constraint_System cs;
    std::ifstream ifs(constraintsFile, std::ifstream::in);
    cs.ascii_load(ifs);
    C_Polyhedron ph(cs);
    Generator_System gs = ph.generators(); // Use ph.minimized_generators() to minimal set of points for the polytope
    bool check = true;
    for(Generator_System::const_iterator it = gs.begin(); it != gs.end(); it++) {
      const Generator& g = *it;
      check = check && g.is_point(); // false for unbounded polyhedra
    }
    std::string outAsString;
    if(check){
      std::stringstream ss;
      gs.ascii_dump(ss);
      outAsString = ss.str();
    }else{
      outAsString = "unbounded";
    }
    char* out = new char[outAsString.size() + 1]; // https://stackoverflow.com/questions/347949/how-to-convert-a-stdstring-to-const-char-or-char
    std::copy(outAsString.begin(), outAsString.end(), out);
    out[outAsString.size()] = '\0'; // don't forget the terminating 0
    return out;
}

}
