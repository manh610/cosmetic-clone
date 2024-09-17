package com.cosmetic.gg.repository.order;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.order.OrderItem;

@Repository
public interface OrderItemRepository extends JpaRepository<OrderItem, String>{

	@Query(value = "SELECT * FROM order_item t WHERE t.order_id=:id", nativeQuery = true)
	List<OrderItem> findByOrder(@Param("id") String id);
	
	@Query(value = "SELECT * FROM order_item t WHERE t.product_item_id=:id", nativeQuery = true)
	Integer cntQuantity(@Param("id") String id);
}
