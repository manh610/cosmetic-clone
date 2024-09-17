package com.cosmetic.gg.repository.order;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.order.OrderProduct;

@Repository
public interface OrderRepository extends JpaRepository<OrderProduct, String>{

	@Query(value = "SELECT * FROM order_product t INNER JOIN address t2 ON t.address_id=t2.id WHERE " +
			"(CASE WHEN :userId IS NOT NULL THEN t.user_id=:userId ELSE (1=1) END) AND " +
			"(CASE WHEN :discountId IS NOT NULL THEN t.discount_id=:discountId ELSE (1=1) END) AND " +
			"(CASE WHEN :deliveryUnitId IS NOT NULL THEN t.delivery_unit_id=:deliveryUnitId ELSE (1=1) END) AND " +
			"(CASE WHEN :censor IS NOT NULL THEN t.censor=:censor ELSE (1=1) END) AND " +
			"(CASE WHEN :shipper IS NOT NULL THEN t.shipper=:shipper ELSE (1=1) END) AND " +
			"(CASE WHEN :deliveryType IS NOT NULL THEN t.delivery_type=:deliveryType ELSE (t.delivery_type <> '') END) AND " +
			"(CASE WHEN COALESCE(:status, NULL) IS NOT NULL THEN " +
		    	"(t.status IN (:status)) ELSE (t.id IS NOT NULL) END) AND " +
			"(CASE WHEN :isPayment IS NOT NULL THEN t.is_payment=:isPayment ELSE (1=1) END) AND " +
		    "(CASE WHEN (:orderDateFrom IS NOT NULL AND :orderDateTo IS NOT NULL) THEN " +
				"(t.order_date>=:orderDateFrom AND t.order_date<=:orderDateTo) ELSE (t.id IS NOT NULL) END) AND " +
		    "(CASE WHEN :provinceId IS NOT NULL THEN t2.province_id=:provinceId ELSE (1=1) END) AND " +
		    "(CASE WHEN :districtId IS NOT NULL THEN t2.district_id=:districtId ELSE (1=1) END) AND " +
		    "(CASE WHEN :wardId IS NOT NULL THEN t2.ward_id=:wardId ELSE (1=1) END) " +
		    "ORDER BY t.order_date DESC LIMIT :pageSize OFFSET :pageIndex"
			, nativeQuery = true)
	List<OrderProduct> search(
			@Param("userId") String userId,
			@Param("discountId") String discountId,
			@Param("deliveryUnitId") String deliveryUnitId,
			@Param("censor") String censor,
			@Param("shipper") String shipper,
			@Param("deliveryType") String deliveryType,
			@Param("status") List<String> status,
			@Param("isPayment") Boolean isPayment,
			@Param("orderDateFrom") LocalDateTime orderDateFrom,
			@Param("orderDateTo") LocalDateTime orderDateTo,
			@Param("provinceId") String provinceId,
			@Param("districtId") String districtId,
			@Param("wardId") String wardId,
			@Param("pageIndex") Integer pageIndex,
            @Param("pageSize") Integer pageSize);
	
	@Query(value = "SELECT count(*) FROM order_product t INNER JOIN address t2 ON t.address_id=t2.id WHERE " +
			"(CASE WHEN :userId IS NOT NULL THEN t.user_id=:userId ELSE (1=1) END) AND " +
			"(CASE WHEN :discountId IS NOT NULL THEN t.discount_id=:discountId ELSE (1=1) END) AND " +
			"(CASE WHEN :deliveryUnitId IS NOT NULL THEN t.delivery_unit_id=:deliveryUnitId ELSE (1=1) END) AND " +
			"(CASE WHEN :censor IS NOT NULL THEN t.censor=:censor ELSE (1=1) END) AND " +
			"(CASE WHEN :shipper IS NOT NULL THEN t.shipper=:shipper ELSE (1=1) END) AND " +
			"(CASE WHEN :deliveryType IS NOT NULL THEN t.delivery_type=:deliveryType ELSE (t.delivery_type <> '') END) AND " +
			"(CASE WHEN COALESCE(:status, NULL) IS NOT NULL THEN " +
		    	"(t.status IN (:status)) ELSE (t.id IS NOT NULL) END) AND " +
			"(CASE WHEN :isPayment IS NOT NULL THEN t.is_payment=:isPayment ELSE (1=1) END) AND " +
		    "(CASE WHEN (:orderDateFrom IS NOT NULL) AND (:orderDateTo IS NOT NULL) THEN " +
			"(t.order_date BETWEEN :orderDateFrom AND :orderDateTo) ELSE (t.id IS NOT NULL) END) AND " +
			"(CASE WHEN :provinceId IS NOT NULL THEN t2.province_id=:provinceId ELSE (1=1) END) AND " +
		    "(CASE WHEN :districtId IS NOT NULL THEN t2.district_id=:districtId ELSE (1=1) END) AND " +
		    "(CASE WHEN :wardId IS NOT NULL THEN t2.ward_id=:wardId ELSE (1=1) END) "
			, nativeQuery = true)
	Integer cntOrder(
			@Param("userId") String userId,
			@Param("discountId") String discountId,
			@Param("deliveryUnitId") String deliveryUnitId,
			@Param("censor") String censor,
			@Param("shipper") String shipper,
			@Param("deliveryType") String deliveryType,
			@Param("status") List<String> status,
			@Param("isPayment") Boolean isPayment,
			@Param("orderDateFrom") LocalDateTime orderDateFrom,
			@Param("orderDateTo") LocalDateTime orderDateTo,
			@Param("provinceId") String provinceId,
			@Param("districtId") String districtId,
			@Param("wardId") String wardId);
	
	@Query(value = "SELECT t.id AS id, t.address_id AS addressId, t.delivery_type AS deliveryType, t.payment_id AS paymentId, " +
			"t.order_date AS orderDate, t.delivery_date AS deliveryDate, t.receipt_date AS receiptDate, t.censor AS censorId, " +
			"t.shipper AS shipperId, t.is_payment AS isPayment, t.note AS note, t.status AS status, t.total_price AS totalPrice, " +
			"t.delivery_unit_id AS deliveryUnitId, t.user_id AS userId, t.discount_id AS discountId, " +
			"t3.full_name AS provinceName, t4.full_name AS districtName, t5.full_name AS wardId, t2.detail AS addressDetail " +
			"FROM order_product t INNER JOIN address t2 ON t.address_id=t2.id " +
			"INNER JOIN provinces t3 ON t3.id=t2.province_id " +
			"INNER JOIN districts t4 ON t4.id=t2.district_id " +
			"INNER JOIN wards t5 ON t5.id=t2.ward_id " +
			"INNER JOIN user t6 ON t6.id=t.user_id " +
			"WHERE t.id=:id "
			, nativeQuery = true)
	Object detail(@Param("id") String id);
	
	@Query(value = "SELECT * FROM order_product t WHERE t.user_id=:id", nativeQuery = true)
	List<OrderProduct> getOrderByUser(@Param("id") String id);
}
