package com.cosmetic.gg.repository.product;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.product.Cart;

@Repository
public interface CartRepository extends JpaRepository<Cart, String>{

	@Query(value = "SELECT * FROM cart t WHERE (t.id = :key OR t.product_item_id = :key OR t.user_id = :key)", nativeQuery = true)
	Cart findByKey(@Param("key") String key);
	
	@Query(value = "SELECT * FROM cart t WHERE (t.user_id=:id) ORDER BY t.created_at ASC ", nativeQuery = true)
	List<Cart> getByUser(@Param("id") String id);
	
	@Query(value = "SELECT * FROM cart t WHERE (t.product_item_id=:productItemId AND t.user_id=:userId)", nativeQuery = true)
	Cart checkExist(@Param("productItemId") String productItemId, @Param("userId") String userId);
}
