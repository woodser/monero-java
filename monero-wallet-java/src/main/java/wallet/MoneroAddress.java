package wallet;

public class MoneroAddress {

  private String standardAddress;

  public MoneroAddress(String standardAddress) {
    super();
    this.standardAddress = standardAddress;
  }

  public String getStandardAddress() {
    return standardAddress;
  }
  
  public String toString() {
    return standardAddress;
  }
}
