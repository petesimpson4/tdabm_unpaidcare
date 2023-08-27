#include <Rcpp.h>
#include <vector>
#include <iostream>
#include <cmath>
using namespace Rcpp;
using namespace std;

/*
library(Rcpp)
sourceCpp('BallMapper.cpp')

pts <- as.data.frame( read.csv('../circle') )
values = pts[,1]

BallMapperCpp <- function( points , values , epsilon )
{
  output <- SimplifiedBallMapperCppInterface( points , values , epsilon )
  colnames(output$vertices) = c('id','size')
  return_list <- output
}#BallMapperCpp

BallMapperCpp( pts,values,0.3 )
*/


inline double compute_distance
  ( const std::vector< NumericVector >& pts , size_t f , size_t s ,  double p = 2 )
{
  double result = 0;
  for ( size_t i = 0 ; i != pts.size() ; ++i )
  {
     result += pow( ( pts[i][f]-pts[i][s] ) , p );
  }
  return pow(result,1/p);
}//Euclidean_distance
//
//
//
//
//
//
//
//
//
void compute_landmarks
(
 const std::vector< NumericVector >& points ,
 std::vector< std::vector<size_t> >& coverage,
 std::vector< size_t >& landmarks ,
 double epsilon , int number_of_points
)
{
  bool dbg = false;
  int first_not_covered_point = 0;
  int number_of_landmark = -1;
  while ( first_not_covered_point < number_of_points )
  {
     if (dbg) Rcerr << "first_not_covered_point : " << first_not_covered_point << endl;
     landmarks.push_back( first_not_covered_point+1 );//!!!
     ++number_of_landmark;
     //check what is covered by points_vec[first_not_covered_point]:
     for ( int i = 0 ; i != number_of_points ; ++i )
     {
       if ( compute_distance( points , first_not_covered_point , i ) <= epsilon )
       {
         coverage[i].push_back( number_of_landmark+1 );
       }
     }
     //now find the next not covered point:
     while ( (first_not_covered_point!=number_of_points) && (coverage[first_not_covered_point].size() != 0) )
     {
       ++first_not_covered_point;
     }
  }
}
//
//
//
//
//
//
//
//
//
// [[Rcpp::export]]
List BallMapperCppInterface( const DataFrame& points_df, const DataFrame& values_df , double epsilon  )
{
  bool dbg = false;
  if ( points_df.size() == 0 )
  {
    cerr << "No points in the BallMapperCpp procedure, the program will now terminate";
    throw "No points in the BallMapperCpp procedure, the program will now terminate";
  }

  std::vector< NumericVector > points( points_df.size() );
  for ( int i = 0 ; i != points_df.size() ; ++i )
  {
    points[i] = points_df[i];
  }
  int number_of_points = points[0].size();
  if (dbg) Rcerr << "Number of points : " << number_of_points << endl;

  NumericVector values = values_df[0];
  if (dbg) Rcerr << "values.size() : " << values.size() << endl;

  std::vector< std::vector<size_t> > coverage( number_of_points );
  std::vector< size_t > landmarks;
  landmarks.reserve( (size_t)(0.2*number_of_points) );

  //here we outsource computations of landmark points:
  compute_landmarks( points , coverage, landmarks , epsilon , number_of_points );

  if (dbg) Rcerr << "landmarks.size() : " << landmarks.size() << endl;
  if (dbg) Rcerr << "coverage.size() : " << coverage.size() << endl;

  if (dbg)
  {
    Rcerr << "Here are the landmarks: \n";
    for ( size_t i = 0 ; i != landmarks.size() ; ++i )
    {
      Rcerr << landmarks[i] << " , ";
    }
  }

  //Now we will compute points_covered_by_landmarks. Firstly let us initialize all the structures:
  NumericVector numer_of_covered_points( landmarks.size() , 2 );//We initialize it to 2, as othervise we get very unbalanced balls sizes.
  for ( size_t i = 0 ; i != coverage.size() ; ++i )
  {
    for ( size_t j = 0 ; j != coverage[i].size() ; ++j )
    {
      numer_of_covered_points[ coverage[i][j]-1 ]++;
    }
  }

  std::vector< std::vector< int > > points_covered_by_landmarks( landmarks.size() );
  for ( size_t i = 0 ; i != landmarks.size() ; ++i )
  {
    std::vector<int> aa;
    aa.reserve( numer_of_covered_points[i] );
    points_covered_by_landmarks[i] = aa;
  }
  //now when the variables are initialized, we can fill in the numer_of_covered_points list:
  for ( size_t i = 0 ; i != coverage.size() ; ++i )
  {
    for ( size_t j = 0 ; j != coverage[i].size() ; ++j )
    {
      points_covered_by_landmarks[ coverage[i][j]-1 ].push_back( i+1 );
    }
  }
  if (dbg) Rcerr << "points_covered_by_landmarks.size() : " << points_covered_by_landmarks.size() << endl;








  //now let us deal with the coloring of the vertices:
  std::vector< double > coloring( points_covered_by_landmarks.size() , 0 );
  for ( size_t i = 0 ; i != points_covered_by_landmarks.size() ; ++i )
  {
     double av = 0;
     for ( size_t j = 0 ; j != points_covered_by_landmarks[i].size() ; ++j )
     {
       av += values[ points_covered_by_landmarks[i][j]-1 ];
     }
     av = av / (double)points_covered_by_landmarks[i].size();
     coloring[i] = av;
  }

  if (dbg)
  {
    Rcerr << "Here is the coloring : \n";
    for ( size_t i = 0 ; i != coloring.size() ; ++i )
    {
      Rcerr << coloring[i] << " , ";
    }
  }

  //Now let us create the graph and record the strength of each edge. To measure this, we will create the incidence matrix of the graph.
  std::vector< std::vector< int > > graph_incidence_matrix( landmarks.size() );
  for ( size_t i = 0 ; i != landmarks.size() ; ++i )
  {
    graph_incidence_matrix[i] = std::vector<int>( i );
  }


  if (dbg) Rcerr << "coverage.size() : " << coverage.size() << endl;

  for ( size_t i = 0 ; i < coverage.size() ; ++i )
  {
    for ( size_t j = 0 ; j < coverage[i].size() ; ++j )
    {
      for ( size_t k = j+1 ; k < coverage[i].size() ; ++k )
      {
        //note that the landmarks in coverage are sorted from smallest to largest
        //therefore we only need this:
        //Rcerr << coverage[i][k] << " " << coverage[i][j] << endl;
        graph_incidence_matrix[ coverage[i][k]-1 ][ coverage[i][j]-1  ]++;
      }
    }
  }


  //first let us count the number of edges in the graph:
  int number_of_edges = 0;
  for ( size_t i = 0 ; i != graph_incidence_matrix.size() ; ++i )
  {
    for ( size_t j = 0 ; j != graph_incidence_matrix[i].size() ; ++j )
    {
       if ( graph_incidence_matrix[i][j] != 0 )++number_of_edges;
    }
  }
  if (dbg)Rcerr << "Number of edges in the graph : " << number_of_edges << endl;

  NumericVector from(number_of_edges);
  NumericVector to(number_of_edges);
  NumericVector strength_of_edges(number_of_edges);

  int edg_no = 0;

  for ( size_t i = 0 ; i != graph_incidence_matrix.size() ; ++i )
  {
    for ( size_t j = 0 ; j != graph_incidence_matrix[i].size() ; ++j )
    {
      if ( graph_incidence_matrix[i][j] != 0 )
      {
         from[edg_no] = j+1;
         to[edg_no] = i+1;
         strength_of_edges[edg_no] = graph_incidence_matrix[i][j];
         ++edg_no;
      }
    }
  }

  NumericVector verts( landmarks.size() );
  for ( size_t i = 0 ; i != landmarks.size() ; ++i )
  {
     verts[i] = i+1;
  }

  List ret;
  ret["vertices"] = cbind(verts,numer_of_covered_points);
  ret["edges"] = cbind(from,to);
  ret["strength_of_edges"] = strength_of_edges;
  ret["points_covered_by_landmarks"] = points_covered_by_landmarks;
  ret["landmarks"] = landmarks;
  ret["coloring"] = coloring;
  ret["coverage"] = coverage;
  //ret["numer_of_covered_points"] = numer_of_covered_points;
  return ret;
}
//
//
//
//
//
//
//
//
//
std::vector< size_t > computeLandmarksForAPointCloud( const DataFrame& points_df , double epsilon  )
{
  bool dbg = false;
  if ( points_df.size() == 0 )
  {
    cerr << "No points in the BallMapperCpp procedure, the program will now terminate";
    throw "No points in the BallMapperCpp procedure, the program will now terminate";
  }

  std::vector< NumericVector > points( points_df.size() );
  for ( int i = 0 ; i != points_df.size() ; ++i )
  {
    points[i] = points_df[i];
  }
  int number_of_points = points[0].size();
  if (dbg) Rcerr << "Number of points : " << number_of_points << endl;

  std::vector< std::vector<size_t> > coverage( number_of_points );
  std::vector< size_t > landmarks;
  landmarks.reserve( (size_t)(0.2*number_of_points) );

  //here we outsource computations of landmark points:
  compute_landmarks( points , coverage, landmarks , epsilon , number_of_points );
  return landmarks;
}
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
inline double compute_distance_standard_points
  ( const std::vector< double >& pt1 , const std::vector< double >& pt2 , double p = 2 )
{
  double result = 0;
  for ( size_t i = 0 ; i != pt1.size() ; ++i )
  {
     result += pow( ( pt1[i]-pt2[i] ) , p );
  }
  return pow(result,1/p);
}//compute_distance_standard_points

void compute_landmarks_not_transposed_pts( std::vector< std::vector<size_t> >& coverage ,
                                           std::vector< size_t > & landmarks ,
                                           const std::vector< std::vector<double> >& points ,
                                           double epsilon
                                         )
{
   //here we outsource computations of landmark points:
  size_t current_point = 0;
  size_t current_landmark = 0;
  while ( true )
  {
      while ( (current_point != points.size()) && (coverage[current_point].size() != 0) )
      {
          ++current_point;
      }
      if ( current_point == points.size() )break;
      landmarks.push_back( current_point );
      for ( size_t j = 0 ; j != points.size() ; ++j )
      {
          if ( compute_distance_standard_points( points[j] , points[current_point] ) <= epsilon )
          {
              coverage[j].push_back( current_landmark+1 );
          }
      }
      current_landmark++;
  }
}

std::vector< std::vector<double> > transpose_points_from_R( const DataFrame& points_df )
{
    std::vector< NumericVector > points_transposed( points_df.size() );
    for ( int i = 0 ; i != points_df.size() ; ++i )
    {
        points_transposed[i] = points_df[i];
    }

    std::vector< std::vector<double> > points( points_transposed[0].size() );

    size_t dim_of_points = points_transposed.size();
    for ( int i = 0 ; i != points_transposed[0].size() ; ++i )
    {
        std::vector< double > pt( dim_of_points );
        for ( size_t j = 0 ; j != dim_of_points ; ++j )
        {
         pt[ j ] = points_transposed[j][i];
        }
        points[i] = pt;
    }
    return points;
}


// [[Rcpp::export]]
List SimplifiedBallMapperCppInterface( const DataFrame& points_df , const DataFrame& values_df , double epsilon  )
{
  bool dbg = false;
  if ( points_df.size() == 0 )
  {
    Rcerr << "No points in the BallMapperCpp procedure, the program will now terminate";
    throw "No points in the BallMapperCpp procedure, the program will now terminate";
  }

  //the points we obtain from R are unnaturally transposed, so we transpose them back to the situation when we
  //have one point per row.
  //First we need to store them as vector of NumericVectorS:
  std::vector< std::vector<double> > points = transpose_points_from_R( points_df );

  int number_of_points = points.size();
  if (dbg) Rcerr << "Number of points : " << number_of_points << endl;

  NumericVector values = values_df[0];
  if (dbg) Rcerr << "values.size() : " << values.size() << endl;


  std::vector< std::vector<size_t> > coverage( number_of_points );
  std::vector< size_t > landmarks;
  landmarks.reserve( (size_t)(0.2*number_of_points) );

  compute_landmarks_not_transposed_pts( coverage , landmarks , points , epsilon );


  if (dbg) Rcerr << "landmarks.size() : " << landmarks.size() << endl;
  if (dbg) Rcerr << "coverage.size() : " << coverage.size() << endl;

  if (dbg)
  {
    Rcerr << "Here are the landmarks: \n";
    for ( size_t i = 0 ; i != landmarks.size() ; ++i )
    {
      Rcerr << landmarks[i] << " , ";
    }
  }

  //Now we will compute points_covered_by_landmarks. Firstly let us initialize all the structures:
  NumericVector numer_of_covered_points( landmarks.size() , 2 );//We initialize it to 2, as othervise we get very unbalanced balls sizes.
  for ( size_t i = 0 ; i != coverage.size() ; ++i )
  {
    for ( size_t j = 0 ; j != coverage[i].size() ; ++j )
    {
        numer_of_covered_points[ coverage[i][j]-1 ]++;
    }
    //Rcerr << endl;
  }

  std::vector< std::vector< int > > points_covered_by_landmarks( landmarks.size() );
  for ( size_t i = 0 ; i != landmarks.size() ; ++i )
  {
    std::vector<int> aa;
    aa.reserve( numer_of_covered_points[i] );
    points_covered_by_landmarks[i] = aa;
  }

  //now when the variables are initialized, we can fill in the numer_of_covered_points list:
  for ( size_t i = 0 ; i != coverage.size() ; ++i )
  {
    for ( size_t j = 0 ; j != coverage[i].size() ; ++j )
    {
        points_covered_by_landmarks[ coverage[i][j]-1 ].push_back( i+1 );
    }
  }
  if (dbg) Rcerr << "points_covered_by_landmarks.size() : " << points_covered_by_landmarks.size() << endl;


  //now let us deal with the coloring of the vertices:
  std::vector< double > coloring( points_covered_by_landmarks.size() , 0 );
  for ( size_t i = 0 ; i != points_covered_by_landmarks.size() ; ++i )
  {
     double av = 0;
     for ( size_t j = 0 ; j != points_covered_by_landmarks[i].size() ; ++j )
     {
       av += values[ points_covered_by_landmarks[i][j]-1 ];
     }
     av = av / (double)points_covered_by_landmarks[i].size();
     coloring[i] = av;
  }

  if (dbg)
  {
    Rcerr << "Here is the coloring : \n";
    for ( size_t i = 0 ; i != coloring.size() ; ++i )
    {
      Rcerr << coloring[i] << " , ";
    }
  }

  //Now let us create the graph and record the strength of each edge. To measure this, we will create the incidence matrix of the graph.
  std::vector< std::vector< int > > graph_incidence_matrix( landmarks.size() );
  for ( size_t i = 0 ; i != landmarks.size() ; ++i )
  {
    graph_incidence_matrix[i] = std::vector<int>( i );
  }


  if (dbg) Rcerr << "coverage.size() : " << coverage.size() << endl;

  for ( size_t i = 0 ; i < coverage.size() ; ++i )
  {
    for ( size_t j = 0 ; j < coverage[i].size() ; ++j )
    {
      for ( size_t k = j+1 ; k < coverage[i].size() ; ++k )
      {
        //note that the landmarks in coverage are sorted from smallest to largest
        //therefore we only need this:
        //Rcerr << coverage[i][k] << " " << coverage[i][j] << endl;
        graph_incidence_matrix[ coverage[i][k]-1 ][ coverage[i][j]-1  ]++;
      }
    }
  }


  //first let us count the number of edges in the graph:
  int number_of_edges = 0;
  for ( size_t i = 0 ; i != graph_incidence_matrix.size() ; ++i )
  {
    for ( size_t j = 0 ; j != graph_incidence_matrix[i].size() ; ++j )
    {
       if ( graph_incidence_matrix[i][j] != 0 )++number_of_edges;
    }
  }
  if (dbg)Rcerr << "Number of edges in the graph : " << number_of_edges << endl;

  NumericVector from(number_of_edges);
  NumericVector to(number_of_edges);
  NumericVector strength_of_edges(number_of_edges);

  int edg_no = 0;

  for ( size_t i = 0 ; i != graph_incidence_matrix.size() ; ++i )
  {
    for ( size_t j = 0 ; j != graph_incidence_matrix[i].size() ; ++j )
    {
      if ( graph_incidence_matrix[i][j] != 0 )
      {
         from[edg_no] = j+1;
         to[edg_no] = i+1;
         strength_of_edges[edg_no] = graph_incidence_matrix[i][j];
         ++edg_no;

      }
    }
  }

  NumericVector verts( landmarks.size() );
  for ( size_t i = 0 ; i != landmarks.size() ; ++i )
  {
     verts[i] = i+1;
  }

  List ret;
  ret["vertices"] = cbind(verts,numer_of_covered_points);
  ret["edges"] = cbind(from,to);
  ret["strength_of_edges"] = strength_of_edges;
  ret["points_covered_by_landmarks"] = points_covered_by_landmarks;
  ret["landmarks"] = landmarks;
  ret["coloring"] = coloring;
  ret["coverage"] = coverage;
  return ret;

return 1;
}



/*
To get the landmark points in R:
aa <- compute_landmark_indices(pts,0.4)
aa = t(aa)
lands <- pts[aa,]
and then they can be used to call the next subroutine:
*/
//[[Rcpp::export]]
NumericVector compute_landmark_indices
(
 const std::vector< NumericVector >& points ,
 double epsilon
)
{
  int number_of_points = points[0].size();
  bool dbg = false;
  int first_not_covered_point = 0;
  int number_of_landmark = -1;

  //let us initialize the landmarks:
  NumericVector landmarks;

  //and coverage:
  std::vector< std::vector<size_t> > coverage( number_of_points );

  while ( first_not_covered_point < number_of_points )
  {
     if (dbg) Rcerr << "first_not_covered_point : " << first_not_covered_point << endl;
     landmarks.push_back( first_not_covered_point+1 );//!!!
     ++number_of_landmark;
     //check what is covered by points_vec[first_not_covered_point]:
     for ( int i = 0 ; i != number_of_points ; ++i )
     {
       if ( compute_distance( points , first_not_covered_point , i ) <= epsilon )
       {
         coverage[i].push_back( number_of_landmark+1 );
       }
     }
     //now find the next not covered point:
     while ( (first_not_covered_point!=number_of_points) && (coverage[first_not_covered_point].size() != 0) )
     {
       ++first_not_covered_point;
     }
  }
  return landmarks;
}

/*
new_epsilon <- computeDistanceBetweenPtClouds(pts,lands)
*/
// [[Rcpp::export]]
double computeDistanceBetweenPtClouds
(
  const DataFrame& pts1_df ,
  const DataFrame& pts2_df
)
{
  std::vector< std::vector<double> > pts1 = transpose_points_from_R( pts1_df );
  std::vector< std::vector<double> > pts2 = transpose_points_from_R( pts2_df );
  double max_distance = 0;
  double d;
  //for every point in one point cloud:
  for ( size_t i = 0 ; i != pts1.size() ; ++i )
  {
      //compute the minimal distance to points from pts2:
      double min_distance = std::numeric_limits<double>::infinity();
      for ( size_t j = 0 ; j != pts2.size() ; ++j )
      {
          d = compute_distance_standard_points( pts1[i] , pts2[j] );
          if ( d < min_distance )min_distance = d;
      }
      if ( min_distance > max_distance )max_distance = min_distance;
  }
  return max_distance;
}//computeDistanceBetweenPtClouds


/*
library(Rcpp)
sourceCpp('BallMapper.cpp')

pts <- as.data.frame( read.csv('../circle') )
values = pts[,1]

aa <- compute_landmark_indices(pts,0.4)
aa = t(aa)
lands <- pts[aa,]



BallMapperLandCpp <- function( points , landmarks , values , epsilon )
{
  output <- SimplifiedBallMapperCppInterfaceBasedOnLands( points , landmarks , values , epsilon )
  colnames(output$vertices) = c('id','size')
  return_list <- output
}#BallMapperLandCpp

BallMapperLandCpp( pts,lands,values,0.4 )

*/
// [[Rcpp::export]]
List SimplifiedBallMapperCppInterfaceBasedOnLands
(
  const DataFrame& points_df ,
  const DataFrame& landmarks_df ,
  const DataFrame& values_df ,
  double epsilon
)
{
  bool dbg = true;
  if ( points_df.size() == 0 )
  {
    Rcerr << "No points in the BallMapperCpp procedure, the program will now terminate";
    throw "No points in the BallMapperCpp procedure, the program will now terminate";
  }

  std::vector< std::vector<double> > points = transpose_points_from_R( points_df );

  int number_of_points = points.size();
  if (dbg) Rcerr << "Number of points : " << number_of_points << endl;

  NumericVector values = values_df[0];
  if (dbg) Rcerr << "values.size() : " << values.size() << endl;

  std::vector< std::vector<double> > landmarks = transpose_points_from_R( landmarks_df );
  if (dbg) Rcerr << "landmarks.size() : " << landmarks.size() << endl;

  std::vector< std::vector<size_t> > coverage( number_of_points );
  for ( size_t i = 0 ; i != points.size() ; ++i )
  {
      for ( size_t j = 0 ; j != landmarks.size() ; ++j )
      {
          if ( compute_distance_standard_points( points[i] , landmarks[j] ) <= epsilon )
          {
              coverage[i].push_back( j+1 );
          }
      }
  }

  //Now we will compute points_covered_by_landmarks. Firstly let us initialize all the structures:
  NumericVector numer_of_covered_points( landmarks.size() , 2 );//We initialize it to 2, as othervise we get very unbalanced balls sizes.
  for ( size_t i = 0 ; i != coverage.size() ; ++i )
  {
    for ( size_t j = 0 ; j != coverage[i].size() ; ++j )
    {
        numer_of_covered_points[ coverage[i][j]-1 ]++;
    }
    //Rcerr << endl;
  }

  std::vector< std::vector< int > > points_covered_by_landmarks( landmarks.size() );
  for ( size_t i = 0 ; i != landmarks.size() ; ++i )
  {
    std::vector<int> aa;
    aa.reserve( numer_of_covered_points[i] );
    points_covered_by_landmarks[i] = aa;
  }

  //now when the variables are initialized, we can fill in the numer_of_covered_points list:
  for ( size_t i = 0 ; i != coverage.size() ; ++i )
  {
    for ( size_t j = 0 ; j != coverage[i].size() ; ++j )
    {
        points_covered_by_landmarks[ coverage[i][j]-1 ].push_back( i+1 );
    }
  }
  if (dbg) Rcerr << "points_covered_by_landmarks.size() : " << points_covered_by_landmarks.size() << endl;


  //now let us deal with the coloring of the vertices:
  std::vector< double > coloring( points_covered_by_landmarks.size() , 0 );
  for ( size_t i = 0 ; i != points_covered_by_landmarks.size() ; ++i )
  {
     double av = 0;
     for ( size_t j = 0 ; j != points_covered_by_landmarks[i].size() ; ++j )
     {
       av += values[ points_covered_by_landmarks[i][j]-1 ];
     }
     av = av / (double)points_covered_by_landmarks[i].size();
     coloring[i] = av;
  }

  if (dbg)
  {
    Rcerr << "Here is the coloring : \n";
    for ( size_t i = 0 ; i != coloring.size() ; ++i )
    {
      Rcerr << coloring[i] << " , ";
    }
  }

  //Now let us create the graph and record the strength of each edge. To measure this, we will create the incidence matrix of the graph.
  std::vector< std::vector< int > > graph_incidence_matrix( landmarks.size() );
  for ( size_t i = 0 ; i != landmarks.size() ; ++i )
  {
    graph_incidence_matrix[i] = std::vector<int>( i );
  }


  if (dbg) Rcerr << "coverage.size() : " << coverage.size() << endl;

  for ( size_t i = 0 ; i < coverage.size() ; ++i )
  {
    for ( size_t j = 0 ; j < coverage[i].size() ; ++j )
    {
      for ( size_t k = j+1 ; k < coverage[i].size() ; ++k )
      {
        //note that the landmarks in coverage are sorted from smallest to largest
        //therefore we only need this:
        //Rcerr << coverage[i][k] << " " << coverage[i][j] << endl;
        graph_incidence_matrix[ coverage[i][k]-1 ][ coverage[i][j]-1  ]++;
      }
    }
  }


  //first let us count the number of edges in the graph:
  int number_of_edges = 0;
  for ( size_t i = 0 ; i != graph_incidence_matrix.size() ; ++i )
  {
    for ( size_t j = 0 ; j != graph_incidence_matrix[i].size() ; ++j )
    {
       if ( graph_incidence_matrix[i][j] != 0 )++number_of_edges;
    }
  }
  if (dbg)Rcerr << "Number of edges in the graph : " << number_of_edges << endl;

  NumericVector from(number_of_edges);
  NumericVector to(number_of_edges);
  NumericVector strength_of_edges(number_of_edges);

  int edg_no = 0;

  for ( size_t i = 0 ; i != graph_incidence_matrix.size() ; ++i )
  {
    for ( size_t j = 0 ; j != graph_incidence_matrix[i].size() ; ++j )
    {
      if ( graph_incidence_matrix[i][j] != 0 )
      {
         from[edg_no] = j+1;
         to[edg_no] = i+1;
         strength_of_edges[edg_no] = graph_incidence_matrix[i][j];
         ++edg_no;

      }
    }
  }

  NumericVector verts( landmarks.size() );
  for ( size_t i = 0 ; i != landmarks.size() ; ++i )
  {
     verts[i] = i+1;
  }

  List ret;
  ret["vertices"] = cbind(verts,numer_of_covered_points);
  ret["edges"] = cbind(from,to);
  ret["strength_of_edges"] = strength_of_edges;
  ret["points_covered_by_landmarks"] = points_covered_by_landmarks;
  ret["landmarks"] = landmarks;
  ret["coloring"] = coloring;
  ret["coverage"] = coverage;
  return ret;

return 1;
}

