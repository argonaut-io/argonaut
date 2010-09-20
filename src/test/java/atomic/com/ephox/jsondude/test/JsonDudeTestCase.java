package com.ephox.jsondude.test;

import com.ephox.circuit.annotations.BeforeLifecycles;
import com.ephox.circuit.annotations.WithWebs;
import com.ephox.circuit.blocks.InjectTestBlock;
import com.ephox.circuit.core.CircuitTestCase;
import com.ephox.jsondude.ioc.JsonDudeWeb;

@WithWebs({JsonDudeWeb.class})
@BeforeLifecycles({InjectTestBlock.class})
public class JsonDudeTestCase extends CircuitTestCase {}
