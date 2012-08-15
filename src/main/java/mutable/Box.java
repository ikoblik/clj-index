package mutable;

import clojure.lang.IDeref;

/**
 * A simple mutable box to store data. Required for trees with cyclic references.
 * 
 * @author Ivan Koblik
 */
public class Box implements IDeref {
    public Object value;

    public Box() {
        value = null;
    }

    public Box(Object value) {
        this.value = value;
    }

    public Object setValue(Object value) {
        this.value = value;
        return value;
    }

    @Override
    public Object deref() {
        return value;
    }

    public String toString() {
        if (null == value) {
            return "";
        }
        return String.valueOf(value);
    }

    public int hashCode() {
        int result = 3137; // just a prime number
        if (null != value) {
            result ^= value.hashCode();
        }
        return result;
    }

    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if (!other.getClass().equals(Box.class)) {
            return false;
        }
        Box oBox = (Box) other;
        return (this.value == oBox.value || (this.value != null && this.value.equals(oBox.value)));
    }
}
