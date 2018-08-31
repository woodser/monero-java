package model;

/**
 * Monero address book entry model.
 */
public class MoneroAddressBookEntry {
  
  private int index;
  private MoneroAddress address;
  private String description;
  
  public MoneroAddressBookEntry(int index, MoneroAddress address, String description) {
    super();
    this.index = index;
    this.address = address;
    this.description = description;
  }

  public int getIndex() {
    return index;
  }

  public void setIndex(int index) {
    this.index = index;
  }

  public MoneroAddress getAddress() {
    return address;
  }

  public void setAddress(MoneroAddress address) {
    this.address = address;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }
}
