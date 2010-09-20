package com.ephox.jsondude.api;

public interface Parser {
    <T> T parse(Class<T> t, String blob);
}
