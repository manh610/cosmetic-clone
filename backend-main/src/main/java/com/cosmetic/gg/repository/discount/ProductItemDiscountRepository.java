package com.cosmetic.gg.repository.discount;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.discount.ProductItemDiscount;

@Repository
public interface ProductItemDiscountRepository extends JpaRepository<ProductItemDiscount, String>{

	@Query( value = "SELECT * FROM product_item_discount t WHERE t.discount_id=:id", nativeQuery = true)
	List<ProductItemDiscount> findAllProductByDiscount(@Param("id") String id);
	
	@Query( value = "SELECT * FROM product_item_discount t WHERE t.discount_id=:id "
			, nativeQuery = true)
	List<ProductItemDiscount> getProductItemByDiscount(@Param("id") String id);
}
