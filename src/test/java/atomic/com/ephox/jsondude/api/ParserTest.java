package com.ephox.jsondude.api;

import com.ephox.jsondude.test.JsonDudeTestCase;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ParserTest extends JsonDudeTestCase {
    Parser subject;

    @Test
    public void test() {
        // FIX 17920 21/09/2010 do something here.....
        String blob = "{'age': 4, 'name': { 'first': 'fred', 'last' : 'gherkin'}, 'friends': ['bob', 'john']}";
        JsonObject actual = subject.parse(JsonObject.class, blob);
        assertEquals(4, (int) actual.get(Integer.class, "age"));
    }
}
