package com.ephox.jsondude.ioc;

import au.net.netstorm.boost.spider.api.config.mapping.Mapper;
import au.net.netstorm.boost.spider.api.config.scope.Scoper;
import au.net.netstorm.boost.spider.api.config.web.Web;

public class JsonDudeWeb implements Web {
    Mapper mapper;
    Scoper scoper;

    public void web() {
        mapper.prefix("Default");
        scoper.scope("com.ephox.jsondude");
    }
}
