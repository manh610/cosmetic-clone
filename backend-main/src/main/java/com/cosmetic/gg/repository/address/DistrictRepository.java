package com.cosmetic.gg.repository.address;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.address.District;

@Repository
public interface DistrictRepository extends JpaRepository<District, String>{
	
	@Query(value = "SELECT * FROM districts t WHERE " +
			"t.province_id = :provinceId " +
			"ORDER BY t.name ASC", nativeQuery = true)
	List<District> findDistricts(@Param("provinceId") String provinceId);
}
