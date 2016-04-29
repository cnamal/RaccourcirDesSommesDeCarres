import java.util.ArrayList;
import java.util.List;

/**
 * Main class
 *
 * @author Namal
 *         Created on 4/29/16.
 */
public class Main {

    public static void main(String[] args){

        // check if the parameters are correct
        if(args.length!=2){
            System.err.println("Requires 2 parameters : initial length and final length");
            System.exit(1);
        }

        try {
            int initialLength = Integer.parseInt(args[0]);
            int finalLength = Integer.parseInt(args[1]);
            Tree res = shorten(initialLength,finalLength);

            //The first node is always 0, therefore we skip it and start printing the children directly
            for(Tree child : res.children){
                List<String> tmp = toStringTrees("",child);
                // We trim because there is a useless space in the beginning
                tmp.forEach(s -> System.out.println(s.trim()));
            }

        }catch (NumberFormatException e){
            //The user type something different than 2 integers
            System.err.println("Requires 2 numbers (integers) : initial length and final length");
            System.exit(1);
        }
    }

    private static Tree shorten(int initialLength, int finalLength) {
        long sum = squaredSum(1,initialLength);
        //The cast is equivalent to a floor
        long threshold = (long) Math.sqrt(squaredSum(finalLength,initialLength));
        return squaredSumTree(0,threshold,finalLength+1,sum);
    }

    private static List<String> toStringTrees(String previous, Tree tree){
        String tmp = previous;
        tmp += " " + tree.node;
        List<String> res = new ArrayList<>();
        if(tree.children.isEmpty()){
            // Leaf node
            res.add(tmp);
            return res;
        }
        for (Tree child : tree.children){
            res.addAll(toStringTrees(tmp,child));
        }
        return res;
    }

    private static long squaredSum(int beg,int end){
        long sum=0;
        for(int i= beg;i<=end;i++)
            sum+=i*i;
        return sum;
    }

    private static Tree squaredSumTree(int beg, long threshold, int length , long squaredSum ){
        // null represents the "ArbreVide" (EmptyTree)

        if(squaredSum<0)
            return null;

        if (length==1 && beg*beg==squaredSum)
            return new Tree(beg);

        if (length==1)
            return null;

        List<Tree> trees = new ArrayList<>();

        for(int i=beg+1;i<= threshold+2-length;i++){
            Tree tmp = squaredSumTree(i,threshold,length-1,squaredSum-beg*beg);
            //The "and" part is replaced by the following verification
            if(tmp != null)
                trees.add(tmp);
        }

        // arbres == 0 = ArbreVide
        if (trees.size()==0)
            return null;

        return new Tree(beg,trees);
    }

}
