package com.ephox.argonaut.j;

import com.ephox.argonaut.Json;

import java.util.List;
import java.util.Map;

public interface JsonFold<T> {
    T null_();
    T boolean_(boolean b);
    T number_(double d);
    T string_(String s);
    T list_(List<Json> l);
    T object_(Map<String, Json> m);
}
