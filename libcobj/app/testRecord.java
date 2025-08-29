package com.example.restservice;
public record testRecord(int statuscode, int param1, double param2, String param3) {
    public testRecord(){
        this(200, 0, 0, "");
    }
}
