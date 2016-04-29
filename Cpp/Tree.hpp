//
// Created by Namal on 4/29/16.
//

#ifndef C_TREE_HPP
#define C_TREE_HPP

#include <vector>

class Tree {
public :
    int node;
    std::vector<Tree*> children;
    Tree(int node) : node(node),children(0){}
    Tree(int node,std::vector<Tree*> children) : node(node),children(children){}

    ~Tree(){
        if(children.size()>0)
            for(Tree* child: children)
                delete child;
    }
};


#endif //C_TREE_HPP
