package com.ephox.jsondude.test;

import com.ephox.circuit.annotations.WithWebs;
import com.ephox.circuit.core.CircuitTestCase;
import com.ephox.jsondude.ioc.JsonDudeWeb;

@WithWebs({JsonDudeWeb.class})
public class JsonDudeTestCase extends CircuitTestCase {}
