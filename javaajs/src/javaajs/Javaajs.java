package javaajs;

import javax.script.*;


public class Javaajs {
    
    public static void main(String[] args) throws Exception {

        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("JavaScript");
        
        InputHolder input = new InputHolder("Input str",42);
        engine.put("_input_", input );

        engine.eval(new java.io.FileReader("test.js"));

        Object o = engine.get("_output_");
        Invocable inv = (Invocable) engine;
        OutputHolder output = inv.getInterface(o, OutputHolder.class);
        
        System.out.println( output.y(23) + " " + output.z() );
    }
}




