#include <iostream>
#include <sstream>
#include <algorithm>
#include "Tree.hpp"

using namespace std;

long squaredSum(int beg,int end){
    long sum=0;
    for(int i= beg;i<=end;i++)
        sum+=i*i;
    return sum;
}

Tree* squaredSumTree(int beg, long threshold, int length , long squaredSum ){
    // null represents the "ArbreVide" (EmptyTree)

    if(squaredSum<0)
        return nullptr;

    if (length==1 && beg*beg==squaredSum)
        return new Tree(beg);

    if (length==1)
        return nullptr;

    vector<Tree*> trees;

    for(int i=beg+1;i<= threshold+2-length;i++){
        Tree* tmp = squaredSumTree(i,threshold,length-1,squaredSum-beg*beg);
        //The "and" part is replaced by the following verification
        if(tmp != nullptr)
            trees.push_back(tmp);
    }

    // arbres == 0 = ArbreVide
    if (trees.size()==0)
        return nullptr;

    return new Tree(beg,trees);
}

Tree* shorten(int initialLength, int finalLength) {
    long sum = squaredSum(1,initialLength);
    //The cast is equivalent to a floor
    long threshold = (long) sqrt(squaredSum(finalLength,initialLength));
    return squaredSumTree(0,threshold,finalLength+1,sum);
}

vector<string> toStringTrees(string previous, Tree* tree){
    string tmp = ( previous + " " + to_string(tree->node));
    vector<string> res;
    if(tree->children.empty()){
        // Leaf node
        res.push_back(tmp);
        return res;
    }
    for (Tree* child : tree->children){
        vector<string> children = toStringTrees(tmp,child);
        res.insert(res.end(),children.begin(),children.end());
    }
    return res;
}

int main(int argc, char* argv[]) {
    if(argc!=3){
        cerr<<"Requires 2 parameters : initial length and final length"<<endl;
        exit(1);
    }

    istringstream ss(argv[1]);
    int initialLength;
    if (!(ss >> initialLength)){
        cerr << "Invalid number " << argv[1] << '\n';
        exit(1);
    }

    ss.str(argv[2]);
    ss.clear();

    int finalLength;
    if (!(ss >> finalLength)){
        cerr << "Invalid number " << argv[2] << '\n';
        exit(1);
    }

    Tree* res = shorten(initialLength,finalLength);

    //The first node is always 0, therefore we skip it and start printing the children directly
    for(Tree* child : res->children){
        vector<string> tmp = toStringTrees("",child);
        // We trim because there is a useless space in the beginning
        for_each(tmp.begin(), tmp.end(), [](string &s){ cout<<s.substr(1)<<endl; });
    }

    delete res;
    return 0;
}