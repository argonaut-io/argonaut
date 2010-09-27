package com.ephox.argonaut.j;

import com.ephox.argonaut.Json;

public interface Interpret<T> {
    T apply(Json j);
}
