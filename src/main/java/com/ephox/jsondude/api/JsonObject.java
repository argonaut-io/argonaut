package com.ephox.jsondude.api;

public interface JsonObject {
    <T> T get(Class<T> t, String key);
}
