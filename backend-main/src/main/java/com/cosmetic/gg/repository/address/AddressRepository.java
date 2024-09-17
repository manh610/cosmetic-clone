package com.cosmetic.gg.repository.address;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.address.Address;

@Repository
public interface AddressRepository extends JpaRepository<Address, String>{
	
	@Query(value = "SELECT * FROM address INNER JOIN user_address t2 ON t2.address_id=address.id " +
			"WHERE (address.is_default = :isDefault AND t2.user_id=:userId )", nativeQuery = true)
	List<Address> findByDefault(@Param("isDefault") boolean isDefault,
								@Param("userId") String userId);
	
	@Query( value = "SELECT * FROM address t WHERE (t.code=:key or t.id=:key) AND t.status = 'ACTIVE'", nativeQuery = true)
	Address findByKey(@Param("key") String key);
	
	@Query(value = "SELECT " +
			"a.id AS id, t2.branch AS branch, a.province_id AS provinceId, a.district_id AS districtId, " +
			"a.ward_id AS wardId, a.detail AS detail, a.status AS status, a.is_default AS isDefault, " +
			"p.full_name AS provinceFullName, d.full_name AS districtFullName, w.full_name AS wardFullName " +
			"FROM address a INNER JOIN supplier_address t2 ON a.id=t2.address_id " +
			"INNER JOIN provinces p ON p.id=a.province_id " +
			"INNER JOIN districts d ON d.id=a.district_id " +
			"INNER JOIN wards w ON w.id=a.ward_id " +
			"WHERE (t2.supplier_id=:supplierId AND a.status='ACTIVE')", nativeQuery = true)
	List<Object> findBySupplier(@Param("supplierId") String supplierId);
	
	@Query(value = "SELECT " +
			"a.id AS id, w.full_name AS wardFullName, a.province_id AS provinceId, a.district_id AS districtId, " +
			"a.ward_id AS wardId, a.detail AS detail, a.status AS status, a.is_default AS isDefault, " +
			"p.full_name AS provinceFullName, d.full_name AS districtFullName, a.full_name AS fullName, " +
			"a.phone AS phone, a.address_type AS addressType " +
			"FROM address a INNER JOIN user_address t2 ON a.id=t2.address_id " +
			"INNER JOIN provinces p ON p.id=a.province_id " +
			"INNER JOIN districts d ON d.id=a.district_id " +
			"INNER JOIN wards w ON w.id=a.ward_id " +
			"WHERE (t2.user_id=:userId AND a.status='ACTIVE')", nativeQuery = true)
	List<Object> findByUser(@Param("userId") String userId);
	
	@Query(value = "SELECT * FROM address t WHERE " +
			"(CASE WHEN :provinceId IS NOT NULL AND :provinceId <> '' THEN " +
		    "(t.province_id = :provinceId) ELSE (t.province_id IS NOT NULL) END) AND " +
		    "(CASE WHEN :districtId IS NOT NULL AND :districtId <> '' THEN " +
		    "(t.district_id = :districtId) ELSE (t.district_id IS NOT NULL) END) AND " +
		    "(CASE WHEN :wardId IS NOT NULL AND :wardId <> '' THEN " +
		    "(t.ward_id = :wardId) ELSE (t.ward_id IS NOT NULL) END) AND " +
		    "(t.status = :status) AND " +
			"(CASE WHEN :code IS NOT NULL AND :code <> '' THEN " +
			"t.code REGEXP :code " +
			"ELSE (t.id IS NOT NULL) END)", nativeQuery = true)
	List<Address> search(@Param("code") String code,
			@Param("status") String status,
			@Param("provinceId") String provinceId,
			@Param("districtId") String districtId,
			@Param("wardId") String wardId);
}
