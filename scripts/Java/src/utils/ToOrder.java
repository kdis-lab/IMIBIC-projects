/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package utils;

/**
 *
 * @author Administrador
 */
public class ToOrder implements Comparable<ToOrder> {

    private double value;
    private int pos;

    public ToOrder(double value, int pos) {
        this.value = value;
        this.pos = pos;
    }

    public double getValue() {
        return value;
    }

    public void setValue(double value) {
        this.value = value;
    }

    public void setPos(int pos) {
        this.pos = pos;
    }

    @Override
    public int compareTo(ToOrder o) {

        if (value > o.value) {
            return 1;
        }

        if (value < o.value) {
            return -1;
        }

        return 0;
    }

    public int getPos() {
        return pos;
    }

    public String toString() {
        return pos + "";
    }

}
