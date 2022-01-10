package Simulations

import Simulations.BasicCloudSimPlusExample.config
import org.cloudbus.cloudsim.allocationpolicies.VmAllocationPolicySimple
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BasicTest5 extends AnyFlatSpec with Matchers {
  behavior of "configuration parameters module"
  it should "obtain a DataCenter" in {
    assert(BasicCloudSimPlusExample.createDatacenter().getCharacteristics().getCostPerSecond==2)
  }
}
