import java.util.ArrayList;
import java.util.List;

/**
 * Tree class
 *
 * @author Namal
 *         Created on 4/29/16.
 */
class Tree {

    int node;
    List<Tree> children;

    /**
     *
     * @param node current node
     */
    Tree(int node){
        this.node = node;
        children = new ArrayList<>();
    }

    /**
     * Initialize with children
     * @param node current node
     * @param children children nodes
     */
    Tree(int node,List<Tree> children){
        this.node = node;
        this.children = children;
    }

}
