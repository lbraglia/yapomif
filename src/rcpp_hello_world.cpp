#include <Rcpp.h>
using namespace Rcpp;

//' This is a test function for Rcpp
//' @export
// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, -3.5 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}
