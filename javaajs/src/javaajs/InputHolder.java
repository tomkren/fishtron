package javaajs;

/**
 *
 * @author Tomáš Křen
 */
public class InputHolder{
    private String str;
    private int    i;
    
    public InputHolder(String _str,int _i){
        str = _str;
        i   = _i;
    }
   
    
    public void setStr(String _str){
        str = _str;
    }
    public String getStr(){
        return str;
    }
    
    public void setI(int _i){
        i = _i;
    }
    public int getI(){
        return i;
    }
}