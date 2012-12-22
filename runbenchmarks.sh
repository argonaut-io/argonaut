#!/bin/sh
sbt ";clean ;compile ;project benchmark; clean; compile"; sbt ";project benchmark; run"
