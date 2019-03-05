package common.types;

import java.util.Collection;

/**
 * Base filter.
 */
public interface Filter<T> {

  /**
   * Indicates if the given item meets the criteria of this filter.
   * 
   * @param item is the item to test
   * @return true if the item meets the criteria of this filter, false otherwise
   */
  public boolean meetsCriteria(T item);
  
  /**
   * Returns a new collection comprised of elements from the given collection
   * that meet this filter's criteria.
   * 
   * @param items are the items to filter
   * @return the items that meet this filter's criteria
   */
  public static Collection<Filter<?>> apply(Filter<?> filter, Collection<Filter<?>> items) {
    throw new RuntimeException("Not implemented");
  }
}
