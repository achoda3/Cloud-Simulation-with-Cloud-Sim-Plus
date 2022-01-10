package Simulations

import HelperUtils.{CreateLogger, ObtainConfigReference}
import Simulations.BasicCloudSimPlusExample.{cloudsim, config, utilizationModel}
import org.cloudbus.cloudsim.allocationpolicies.{VmAllocationPolicyRoundRobin, VmAllocationPolicySimple}
import org.cloudbus.cloudsim.brokers.{DatacenterBrokerBestFit, DatacenterBrokerSimple}
import org.cloudbus.cloudsim.cloudlets.network.CloudletSendTask
import org.cloudbus.cloudsim.cloudlets.{Cloudlet, CloudletSimple}
import org.cloudbus.cloudsim.core.CloudSim
import org.cloudbus.cloudsim.datacenters.network.NetworkDatacenter
import org.cloudbus.cloudsim.datacenters.{Datacenter, DatacenterSimple}
import org.cloudbus.cloudsim.distributions.PoissonDistr
import org.cloudbus.cloudsim.hosts.HostSimple
import org.cloudbus.cloudsim.network.topologies.BriteNetworkTopology
import org.cloudbus.cloudsim.provisioners.{PeProvisionerSimple, ResourceProvisionerSimple}
import org.cloudbus.cloudsim.resources.{Pe, PeSimple}
import org.cloudbus.cloudsim.schedulers.cloudlet.{CloudletScheduler, CloudletSchedulerSpaceShared, CloudletSchedulerTimeShared}
import org.cloudbus.cloudsim.schedulers.vm.VmSchedulerTimeShared
import org.cloudbus.cloudsim.utilizationmodels.{UtilizationModel, UtilizationModelDynamic, UtilizationModelFull}
import org.cloudbus.cloudsim.vms.{Vm, VmSimple}
import org.cloudsimplus.builders.tables.CloudletsTableBuilder
import org.cloudsimplus.faultinjection.{HostFaultInjection, VmClonerSimple}
import org.cloudsimplus.listeners.{CloudletVmEventInfo, VmHostEventInfo}
import org.cloudbus.cloudsim.network.topologies.BriteNetworkTopology
import org.cloudbus.cloudsim.network.topologies.NetworkTopology

import java.util.function.BiFunction
import scala.language.postfixOps
import collection.JavaConverters.*

class BasicCloudSimPlusExample

object BasicCloudSimPlusExample:
  val config = ObtainConfigReference("cloudSimulator") match {
    case Some(value) => value
    case None => throw new RuntimeException("Cannot obtain a reference to the config data.")
  }
  val logger = CreateLogger(classOf[BasicCloudSimPlusExample])
  val utilizationModel = new UtilizationModelDynamic(0.5)
  val utilizationModelDynamic = new UtilizationModelDynamic(0.1)
  val utilizationModelFull = new UtilizationModelFull

  // Setting up the Cloud, broker and hostPEs as global so that all functions and parts of the application have access to them
  val cloudsim = new CloudSim()

  //Function that puts a listener on the vms. Helps keep track of CPU Utilization
  def vmProcessingUpdateListener(info: VmHostEventInfo) = {
    val vm = info.getVm
    val Time = info.getTime
    val Host = vm.getHost
    val Husage = vm.getHost.getCpuPercentUtilization * 100
    //Destroys VM 1 when its CPU usage reaches 90%
    if ((vm.getCpuPercentUtilization > 0.9) && vm.isCreated) {
      val usage = vm.getCpuPercentUtilization * 100
      logger.info(s" $Time: Intentionally destroying $vm due to CPU overload. Current VM CPU usage is, $usage")
      vm.getHost.destroyVm(vm)
    }
    logger.info(s" # $Time : $Host CPU Utilization $Husage")
  }

  //Function for cloning cloudlet in case of Faults which we simulate using injections
  def cloneCloudlet(source: Cloudlet) : Cloudlet = {
    val clone = new CloudletSimple(source.getLength, source.getNumberOfPes)
    /*It' not required to set an ID for the clone.
            It is being set here just to make it easy to
            relate the ID of the cloudlet to its clone,
            since the clone ID will be 10 times the id of its
            source cloudlet.*/
    clone.setId(source.getId * 10)
    clone.setUtilizationModel(utilizationModel)
    clone.setLength(config.getLong("cloudSimulator.cloudlet.size"))
    clone.setNumberOfPes(config.getLong("cloudSimulator.cloudlet.PEs"))
    clone
  }

  // Clones the entire set of cloudlets in case of vm failure using injections
  def cloneCloudlets(sourceVm: Vm) =  {
    val sourceVmCloudlets = sourceVm.getCloudletScheduler.getCloudletList.asScala
    val clonedCloudlets = sourceVmCloudlets.map(x => cloneCloudlet(x))
    clonedCloudlets.asJava
  }

  // Clones Vm and makes snapshot of the vm that just got destroyed and assigns it to different host
  def cloneVm (Vm: Vm) = {
    val clonedVm = new VmSimple(config.getLong("cloudSimulator.vm.mipsCapacity").toDouble, config.getInt("cloudSimulation.vm.PEs"))
      .setRam(config.getLong("cloudSimulator.vm.RAMInMBs"))
      .setBw(config.getLong("cloudSimulator.vm.BandwidthInMBps"))
      .setSize(config.getLong("cloudSimulator.vm.StorageInMBs"))
    clonedVm
  }

  // Function to create Fault Injections for hosts randomly during runtime and causes cloning if all the vms are destroyed that are needed by that host
  def createFaultInjectionForHosts(datacenter: Datacenter, broker0: DatacenterBrokerSimple): Unit = { //Use the system time to get random results every time you run the simulation
    //final long seed = System.currentTimeMillis();
    val seed = 112717613L
    var poisson = PoissonDistr(0.0001, seed)
    var Inject: HostFaultInjection = new HostFaultInjection(datacenter, poisson)
    Inject.setMaxTimeToFailInHours(800)
    Inject.addVmCloner(broker0, new VmClonerSimple(this.cloneVm, this.cloneCloudlets))
  }

  // Function that cancels a cloudlet at half its execution
  private def cancelCloudletIfHalfExecuted(e: CloudletVmEventInfo): Unit = {
    val cloudlet = e.getCloudlet
    if (cloudlet.getFinishedLengthSoFar >= config.getLong("cloudSimulator.cloudlet.size") / 2) {
      System.out.printf("%n# %.2f: Intentionally cancelling %s execution after it has executed half of its length.%n", e.getTime, cloudlet)
      cloudlet.getVm.getCloudletScheduler.cloudletCancel(cloudlet)
    }
  }

  // Function to create Datacenter
  def createDatacenter() = {
    val hostPes = List.fill(config.getInt("cloudSimulator.host.PEs"))(new PeSimple(config.getLong("cloudSimulator.host.mipsCapacity").toDouble, new PeProvisionerSimple()))
    val hostList = List.fill(config.getInt("cloudSimulator.data_center.number_of_hosts"))(new HostSimple(config.getLong("cloudSimulator.host.RAMInMBs"),
      config.getLong("cloudSimulator.host.StorageInMBs"),
      config.getLong("cloudSimulator.host.BandwidthInMBps"),
      hostPes.asJava)
      .setVmScheduler(new VmSchedulerTimeShared())
      .setRamProvisioner(new ResourceProvisionerSimple())
      .setBwProvisioner(new ResourceProvisionerSimple()))
    val datacenter:NetworkDatacenter = new NetworkDatacenter(cloudsim, hostList.asJava , new VmAllocationPolicySimple())
    datacenter.getCharacteristics.setCostPerSecond(config.getInt("cloudSimulator.data_center.cost"))
      .setCostPerMem(config.getInt("cloudSimulator.data_center.cost_per_mem"))
      .setCostPerStorage(config.getInt("cloudSimulator.data_center.cost_per_storage"))
      .setCostPerBw(config.getDouble("cloudSimulator.data_center.cost_per_bw"))
    datacenter.setSchedulingInterval(config.getInt("cloudSimulator.data_center.scheduling_interval"))
    datacenter
  }

  // Function to create Vms
  def CreateVms() = {
    List.fill(config.getInt("cloudSimulator.vm.number_of_vms"))(new VmSimple(config.getLong("cloudSimulator.vm.mipsCapacity").toDouble, config.getInt("cloudSimulator.vm.PEs"))
      .setRam(config.getLong("cloudSimulator.vm.RAMInMBs"))
      .setBw(config.getLong("cloudSimulator.vm.BandwidthInMBps"))
      .setSize(config.getLong("cloudSimulator.vm.StorageInMBs"))
      .setCloudletScheduler(new CloudletSchedulerSpaceShared()))
  }

  // Function to Create CloudLets
  def CreateCloudlets() : List[Cloudlet] = {
    val dynamicUtilization = new UtilizationModelDynamic
    val r = scala.util.Random
    val factor = r.nextFloat()
    val chance1 = r.nextInt(100)
    if(chance1>50){
      dynamicUtilization.setUtilizationUpdateFunction((um: UtilizationModelDynamic) => um.getUtilization + (um.getTimeSpan * factor))
    } else {
      dynamicUtilization.setUtilizationUpdateFunction((um: UtilizationModelDynamic) => um.getUtilization + (um.getTimeSpan * -1 * factor))
    }
    List.fill(config.getInt("cloudSimulator.cloudlet.number_of_cloudlets"))(new CloudletSimple(config.getLong("cloudSimulator.cloudlet.size"), config.getInt("cloudSimulator.cloudlet.PEs"))
      .setUtilizationModelCpu(dynamicUtilization)
      .setUtilizationModelBw(utilizationModelDynamic)
      .setUtilizationModelRam(utilizationModelDynamic)
      .setFileSize(config.getLong("cloudSimulator.cloudlet.file_size"))
      .setOutputSize(config.getLong("cloudSimulator.cloudlet.output_size")))
  }

  def Start() =
    //Creating Datacenter
    val dc0 = createDatacenter()
    val dc1 = createDatacenter()
    val broker0 = new DatacenterBrokerSimple(cloudsim)
    // Creating Network Topology
    val networkTopology = new BriteNetworkTopology
    cloudsim.setNetworkTopology(networkTopology)
    //Datacenter0 will correspond to BRITE node 0
    networkTopology.mapNode(dc0, 0)
    networkTopology.mapNode(dc1, 2)
    networkTopology.mapNode(broker0, 3)
    networkTopology.addLink(dc0, broker0, config.getLong("cloudSimulator.network_bw").toDouble, config.getLong("cloudSimulator.network_latency").toDouble)    //Maps CloudSim entities to BRITE entities
    networkTopology.addLink(dc1, broker0, config.getLong("cloudSimulator.network_bw").toDouble, config.getLong("cloudSimulator.network_latency").toDouble)    //Maps CloudSim entities to BRITE entities
    //Creating Vms
    val vmList = CreateVms()
    // Adding Listeners to all Vms to check CPU Utilization
    for(x <- vmList){
      x.addOnUpdateProcessingListener(this.vmProcessingUpdateListener)
    }
    // Creating cloudlet List
    val cloudletList = CreateCloudlets()
    logger.info(s"Created a list of cloudlets: $cloudletList")
    cloudletList(0).addOnUpdateProcessingListener(this.cancelCloudletIfHalfExecuted)
    broker0.submitVmList(vmList.asJava)
    //Alternating the assignments of cloudlets to Vms from the VmList
    for(w <- 0 to config.getInt("cloudSimulator.cloudlet.number_of_cloudlets")-1){
      if(w%2==0){
        broker0.bindCloudletToVm(cloudletList(w), vmList(0))
      } else {
        broker0.bindCloudletToVm(cloudletList(w), vmList(1))
      }
    }

    //Submitting the entire CloudletList
    broker0.submitCloudletList(cloudletList.asJava);
    logger.info("Starting cloud simulation...")
    //Creating faults
    createFaultInjectionForHosts(dc0, broker0)
    //Starting the simulation
    cloudsim.start();

    new CloudletsTableBuilder(broker0.getCloudletFinishedList()).build()