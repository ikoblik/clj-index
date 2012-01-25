package mutable;

/**
 * A simple mutable box to store data. Required for trees with cyclic references.
 *
 */
public class Box {
    Object value;
    
    public Box(){
        value = null;
    }

    public Box(Object value){
        this.value = value;
    }

    public Object setValue(Object value){
        this.value = value;
        return value;
    }

    public Object getValue(){
        return value;
    }
}
