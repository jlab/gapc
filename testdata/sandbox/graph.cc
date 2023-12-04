#include <iostream>
#include <utility>
#include <vector>
#include <list>

#include <cstring>

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>

#include <boost/graph/edge_list.hpp>

typedef std::pair<int, int> Edge;

//typedef boost::adjacency_list<boost::vecS, boost::vecS,
//            boost::bidirectionalS, 
//            boost::property<boost::vertex_color_t, boost::default_color_type>
//          > Graph;

typedef boost::adjacency_list< boost::vecS, boost::vecS, boost::directedS,
        boost::property<boost::vertex_color_t, boost::default_color_type> > Graph;

typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;


typedef std::list<Vertex> TOrdering;

typedef boost::edge_list<std::vector<Edge>::iterator> EdgeGraph;
typedef boost::graph_traits<EdgeGraph>::vertex_descriptor EdgeVertex;
typedef std::list<EdgeVertex> EdgeOrdering;

int main(int argc, char **argv)
{
  std::vector<Edge> edges;
  for (int i = 2; i <argc-1; i+=2) {
    edges.push_back(std::make_pair(std::atoi(argv[i]), std::atoi(argv[i+1])));
  }

  Graph g(edges.begin(), edges.end(), std::atoi(argv[1]));

  EdgeGraph h(edges.begin(), edges.end());


  TOrdering res;
  EdgeOrdering eres;
  
  boost::topological_sort(g, std::front_inserter(res));
  //boost::topological_sort(h, std::front_inserter(eres));

  for (TOrdering::iterator i = res.begin(); i != res.end(); ++i)
    std::cout << *i << std::endl;
  
  //for (TOrdering::iterator i = eres.begin(); i != eres.end(); ++i)
  //  std::cout << *i << std::endl;

  return 0;
}
