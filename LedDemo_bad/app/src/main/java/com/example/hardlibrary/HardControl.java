package com.example.hardlibrary;

public class HardControl{
    public static native int ledCtrl(int which,int status);
    public static native int ledOpen(); //静态方法不需要实例化对象
    public static native void ledClose();

    static {

        try {
            System.loadLibrary("HardControl");  //.c库的文件
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}