#include <Rcpp.h>
#include "compactnesslib/compactnesslib.hpp"

using namespace Rcpp;

std::vector<std::string> getListOfScores() {
  return complib::score_names;
}

std::string getScoresForGeoJSON(const std::string &geojson, const std::string id, const std::vector<std::string> &score_list){
  auto gc = complib::ReadGeoJSON(geojson);
  complib::CalculateListOfScores(gc, score_list);
  return complib::OutScoreJSON(gc, id);
}

void augmentShapefileWithScores(const std::string filename, const std::vector<std::string> &score_list){
  auto gc = complib::ReadShapefile(filename);
  complib::CalculateListOfScores(gc, score_list);
  complib::WriteShapeScores(gc, filename);
}

void addScoresToNewShapefile(const std::string in_filename, const std::string out_filename, const std::vector<std::string> &score_list){
  auto gc = complib::ReadShapefile(in_filename);
  complib::CalculateListOfScores(gc, score_list);
  complib::WriteShapefile(gc,out_filename);
}





RCPP_MODULE(complib) {
  function("getListOfScores",            &getListOfScores);
  function("getScoresForGeoJSON",        &getScoresForGeoJSON);
  function("augmentShapefileWithScores", &augmentShapefileWithScores);
  function("addScoresToNewShapefile",    &addScoresToNewShapefile);
}


/*
List NumericVectorExample(const NumericVector & orig) {
    NumericVector vec(orig.size());     // create a target vector of the same size
    
    // we could query size via
    //   int n = vec.size();
    // and loop over the vector, but using the STL is so much nicer
    // so we use a STL transform() algorithm on each element
    std::transform(orig.begin(), orig.end(), vec.begin(), sqrt_double);

    return List::create(Named("result") = vec,
                        Named("original") = orig);
}*/